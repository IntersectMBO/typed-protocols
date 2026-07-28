{-# LANGUAGE CPP #-}
-- orphaned arbitrary instances
{-# OPTIONS_GHC -Wno-orphans #-}

module Network.TypedProtocol.ReqResp.Tests (tests) where

import Network.TypedProtocol.Channel
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Codec.Properties
import Network.TypedProtocol.Driver.Simple
import Network.TypedProtocol.Proofs

import Network.TypedProtocol.ReqResp.Client
import Network.TypedProtocol.ReqResp.Codec
import Network.TypedProtocol.ReqResp.Codec.CBOR qualified as CBOR
import Network.TypedProtocol.ReqResp.Examples
import Network.TypedProtocol.ReqResp.Server
import Network.TypedProtocol.ReqResp.Type

import Control.Concurrent.Class.MonadSTM.TVar (newTVarIO, readTVar, writeTVar)
import Control.Exception (throw)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTest (MonadTest (exploreRaces))
import Control.Monad.Class.MonadThrow
import Control.Monad.IOSim
import Control.Monad.ST (runST)
import Control.Monad.Trans.State.Strict (State, runState, state)
import Control.Tracer (nullTracer)

import Data.Functor.Identity (Identity (..))
import Data.List (intercalate, mapAccumL)
import Data.Tuple (swap)
#if !defined(mingw32_HOST_OS)
import Network.Socket qualified as Socket
import System.Directory (removeFile)
import System.IO
import System.Posix.Files qualified as Posix
#endif

import Network.TypedProtocol.PingPong.Tests (splits2, splits2BS, splits3,
           splits3BS)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Show.Functions ()


--
-- The list of all properties
--

tests :: TestTree
tests = testGroup "Network.TypedProtocol.ReqResp"
  [ testProperty "direct"              prop_direct
  , testProperty "directPipelined"     prop_directPipelined
  , testProperty "connect"             prop_connect
  , testProperty "connectPipelined"    prop_connectPipelined
  , testProperty "connectLookahead"    prop_connectLookahead
  , testProperty "channel ST"          prop_channel_ST
  , testProperty "channel IO"          prop_channel_IO
  , testProperty "channelPipelined ST" prop_channelPipelined_ST
  , testProperty "channelPipelined IO" prop_channelPipelined_IO
  , testProperty "channelLookahead ST"       prop_channelLookahead_ST
  , testProperty "channelLookahead IO"       prop_channelLookahead_IO
  , testProperty "channelLookahead IOSimPOR" prop_channelLookahead_IOSimPOR
  , testProperty "lookaheadFixedEquiv ST"    prop_lookaheadFixedEquiv_ST
  , testProperty "lookaheadFixedEquiv IO"    prop_lookaheadFixedEquiv_IO
#if !defined(mingw32_HOST_OS)
  , testProperty "namedPipePipelined"  prop_namedPipePipelined_IO
  , testProperty "socketPipelined"     prop_socketPipelined_IO
#endif
  , testGroup "Codec"
    [ testProperty "codec"             prop_codec_ReqResp
    , testProperty "codec 2-splits"    prop_codec_splits2_ReqResp
    , testProperty "codec 3-splits"    (withMaxSuccess 33 prop_codec_splits3_ReqResp)
    , testGroup "CBOR"
      [ testProperty "codec"           prop_codec_cbor_ReqResp
      , testProperty "codec 2-splits"  prop_codec_cbor_splits2_ReqResp
      , testProperty "codec 3-splits"  $ withMaxSuccess 30 prop_codec_cbor_splits3_ReqResp
      ]
    ]
  , testGroup "AnnotatedCodec"
    [ testProperty "codec"             prop_anncodec_ReqResp
    ]
  ]


--
-- Properties going directly, not via Peer.
--

direct :: Monad m
       => ReqRespClient req resp m a
       -> ReqRespServer req resp m b
       -> m (a, b)

direct (SendMsgDone clientResult) ReqRespServer{recvMsgDone} =
    (,) <$> clientResult <*> recvMsgDone

direct (SendMsgReq req kResp) ReqRespServer{recvMsgReq} = do
    (resp, server') <- recvMsgReq req
    client' <- kResp resp
    direct client' server'


directPipelined :: Monad m
                => ReqRespClientPipelined req resp m a
                -> ReqRespServer          req resp m b
                -> m (a, b)
directPipelined (ReqRespClientPipelined client0) server0 =
    go EmptyQ client0 server0
  where
    go :: Monad m
       => Queue n c
       -> ReqRespIdle   req resp n c m a
       -> ReqRespServer req resp     m b
       -> m (a, b)
    go EmptyQ (SendMsgDonePipelined clientResult) ReqRespServer{recvMsgDone} =
      (clientResult,) <$> recvMsgDone

    go q (SendMsgReqPipelined req kResp client') ReqRespServer{recvMsgReq} = do
      (resp, server') <- recvMsgReq req
      x               <- kResp resp
      go (enqueue x q) client' server'

    go (ConsQ resp q) (CollectPipelined _ k) server = do
      client' <- k resp
      go q client' server


prop_direct :: (Int -> Int -> (Int, Int)) -> [Int] -> Bool
prop_direct f xs =
    runIdentity
      (direct
        (reqRespClientMap xs)
        (reqRespServerMapAccumL (\a -> pure . f a) 0))
 ==
    swap (mapAccumL f 0 xs)

prop_directPipelined :: (Int -> Int -> (Int, Int)) -> [Int] -> Bool
prop_directPipelined f xs =
    runIdentity
      (directPipelined
        (reqRespClientMapPipelined xs)
        (reqRespServerMapAccumL (\a -> pure . f a) 0))
 ==
    swap (mapAccumL f 0 xs)


--
-- Properties using connect
--

prop_connect :: (Int -> Int -> (Int, Int)) -> [Int] -> Bool
prop_connect f xs =
    case runIdentity
           (connect
             (reqRespClientPeer (reqRespClientMap xs))
             (reqRespServerPeer (reqRespServerMapAccumL (\a -> pure . f a) 0)))

      of (c, s, TerminalStates SingDone SingDone) ->
           (s, c) == mapAccumL f 0 xs


prop_connectPipelined :: [Bool] -> (Int -> Int -> (Int, Int)) -> [Int] -> Bool
prop_connectPipelined cs f xs =
    case runIdentity
           (connectPipelined cs
             (reqRespClientPeerPipelined (reqRespClientMapPipelined xs))
             (reqRespServerPeer
               (reqRespServerMapAccumL (\a -> pure . f a) 0)))

      of (c, s, TerminalStates SingDone SingDone) ->
           (s, c) == mapAccumL f 0 xs


-- | The dual of 'prop_connectPipelined': a lookahead server (which ignores the
-- request payload and reads each reply from the state) against a non-pipelined
-- client. The result must not depend on the interleaving choices.
--
prop_connectLookahead :: [Bool] -> (Int -> (Int, Int)) -> NonNegative Int -> Bool
prop_connectLookahead cs g (NonNegative n) =
    case runState
           (connectLookahead cs
             (reqRespServerPeerLookahead nextResp)
             (reqRespClientPeer (reqRespClientMap (replicate n ()))))
           0

      of ((_, resps, TerminalStates SingDone SingDone), acc) ->
           (acc, resps) == mapAccumL (\a () -> g a) 0 (replicate n ())
  where
    nextResp :: State Int Int
    nextResp = state (\a -> let (a', r) = g a in (r, a'))


--
-- Properties using channels, codecs and drivers.
--

prop_channel :: ( MonadLabelledSTM m
                , MonadTraceSTM m
                , MonadAsync m
                , MonadCatch m
                , MonadEvaluate m
                )
             => (Int -> Int -> (Int, Int)) -> [Int]
             -> m Bool
prop_channel f xs = do
    (c, s) <- runConnectedPeers createConnectedChannels
                                nullTracer
                                codecReqResp client server
    return ((s, c) == mapAccumL f 0 xs)
  where
    client = reqRespClientPeer (reqRespClientMap xs)
    server = reqRespServerPeer (reqRespServerMapAccumL
                                 (\a -> pure . f a) 0)


prop_channel_IO :: (Int -> Int -> (Int, Int)) -> [Int] -> Property
prop_channel_IO f xs =
    ioProperty (prop_channel f xs)

prop_channel_ST :: (Int -> Int -> (Int, Int)) -> [Int] -> Bool
prop_channel_ST f xs =
    runSimOrThrow (prop_channel f xs)


prop_channelPipelined :: ( MonadLabelledSTM m
                         , MonadAsync m
                         , MonadCatch m
                         , MonadEvaluate m
                         , MonadST m
                         )
                      => (Int -> Int -> (Int, Int)) -> [Int]
                      -> m Bool
prop_channelPipelined f xs = do
    (c, s) <- runConnectedPeersPipelined
                (createPipelineTestChannels 100)
                nullTracer
                CBOR.codecReqResp
                client server
    return ((s, c) == mapAccumL f 0 xs)
  where
    client = reqRespClientPeerPipelined (reqRespClientMapPipelined xs)
    server = reqRespServerPeer          (reqRespServerMapAccumL
                                            (\a -> pure . f a) 0)

prop_channelPipelined_IO :: (Int -> Int -> (Int, Int)) -> [Int] -> Property
prop_channelPipelined_IO f xs =
    ioProperty (prop_channelPipelined f xs)

prop_channelPipelined_ST :: (Int -> Int -> (Int, Int)) -> [Int] -> Property
prop_channelPipelined_ST f xs =
    let tr = runSimTrace (prop_channelPipelined f xs) in
    counterexample (intercalate "\n" $ map show $ traceEvents tr)
                 $ case traceResult True tr of
                     Left  err -> throw err
                     Right res -> res


-- | The channel/driver sibling of 'prop_connectLookahead': a pipelined client
-- against a lookahead server, run through the real 'runPipelinedPeer' /
-- 'runLookaheadPeer' drivers over a channel. Unlike @connect@, this genuinely
-- runs the 'Sender's concurrently with the main peer's receive-ahead, so the
-- lookahead machinery is actually exercised. The collected replies must equal
-- the reference sequence regardless of the interleaving the runtime picks.
--
prop_channelLookahead :: ( MonadLabelledSTM m
                         , MonadAsync m
                         , MonadCatch m
                         , MonadEvaluate m
                         , MonadST m
                         , MonadTest m
                         )
                      => (Int -> (Int, Int)) -> NonNegative Int
                      -> m Bool
prop_channelLookahead g (NonNegative n) = do
    -- mark all threads forked from here on as system threads, so IOSimPOR will
    -- reverse races between the client's receivers and the server's 'Sender's
    -- (a no-op under IO / plain IOSim)
    exploreRaces
    accVar <- newTVarIO 0
    let nextResp = atomically $ do
                     a <- readTVar accVar
                     let (a', r) = g a
                     writeTVar accVar a'
                     return r
        client   = reqRespClientPeerPipelined (reqRespClientMapPipelined (replicate n ()))
        server   = reqRespServerPeerLookahead nextResp
    (resps, ()) <- runConnectedPeersLookahead
                     (createPipelineTestChannels 100)
                     nullTracer
                     CBOR.codecReqResp
                     client server
    return (resps == snd (mapAccumL (\a () -> g a) 0 (replicate n ())))

prop_channelLookahead_IO :: (Int -> (Int, Int)) -> NonNegative Int -> Property
prop_channelLookahead_IO g n =
    ioProperty (prop_channelLookahead g n)

prop_channelLookahead_ST :: (Int -> (Int, Int)) -> NonNegative Int -> Property
prop_channelLookahead_ST g n =
    let tr = runSimTrace (prop_channelLookahead g n) in
    counterexample (intercalate "\n" $ map show $ traceEvents tr)
                 $ case traceResult True tr of
                     Left  err -> throw err
                     Right res -> res

-- | Have IOSimPOR systematically explore the interleavings of the client's
-- receivers and the server's 'Sender's, checking that the lookahead driver
-- produces the reference result under every schedule. The request count is kept
-- small so the schedule space stays tractable.
--
prop_channelLookahead_IOSimPOR :: (Int -> (Int, Int)) -> NonNegative Int -> Property
prop_channelLookahead_IOSimPOR g (NonNegative n) =
    withMaxSuccess 20 $
    exploreSimTrace id (prop_channelLookahead g (NonNegative (min n 6))) $ \_ tr ->
      case traceResult False tr of
        Left  failure -> counterexample (show failure) (property False)
        Right res     -> property res


-- | 'runLookaheadFixedSenderPeerWithDriver' and
-- @'runLookaheadPeerWithDriver' . 'embedLookaheadUsingFixedSender'@ run the same
-- fixed-'Sender' peer two different ways — the former tracking outstanding sends
-- with a counter, the latter with a queue of (degenerate) 'Sender's. This runs
-- a pipelined client against the same fixed server via each and checks both
-- collect the same replies, matching the reference sequence.
--
prop_lookaheadFixedEquiv :: ( MonadLabelledSTM m
                            , MonadAsync m
                            , MonadCatch m
                            , MonadEvaluate m
                            , MonadST m
                            )
                         => (Int -> (Int, Int)) -> NonNegative Int
                         -> m Bool
prop_lookaheadFixedEquiv g (NonNegative n) = do
    viaFixed <- runFixed
    viaEmbed <- runEmbed
    return (viaFixed == reference && viaEmbed == reference)
  where
    reqs      = replicate n ()
    reference = snd (mapAccumL (\a () -> g a) 0 reqs)

    client = reqRespClientPeerPipelined (reqRespClientMapPipelined reqs)

    -- a fresh stateful reply source (so each run starts from the same state)
    mkNextResp = do
      accVar <- newTVarIO 0
      return $ atomically $ do
                 a <- readTVar accVar
                 let (a', r) = g a
                 writeTVar accVar a'
                 return r

    runFixed = do
      nextResp <- mkNextResp
      (resps, ()) <- runConnectedPeersLookaheadFixedSender
                       (createPipelineTestChannels 100) nullTracer CBOR.codecReqResp
                       client (reqRespServerPeerLookaheadFixedSender nextResp)
      return resps

    runEmbed = do
      nextResp <- mkNextResp
      (resps, ()) <- runConnectedPeersLookahead
                       (createPipelineTestChannels 100) nullTracer CBOR.codecReqResp
                       client (embedLookaheadUsingFixedSender (reqRespServerPeerLookaheadFixedSender nextResp))
      return resps

prop_lookaheadFixedEquiv_IO :: (Int -> (Int, Int)) -> NonNegative Int -> Property
prop_lookaheadFixedEquiv_IO g n =
    ioProperty (prop_lookaheadFixedEquiv g n)

prop_lookaheadFixedEquiv_ST :: (Int -> (Int, Int)) -> NonNegative Int -> Property
prop_lookaheadFixedEquiv_ST g n =
    let tr = runSimTrace (prop_lookaheadFixedEquiv g n) in
    counterexample (intercalate "\n" $ map show $ traceEvents tr)
                 $ case traceResult True tr of
                     Left  err -> throw err
                     Right res -> res


#if !defined(mingw32_HOST_OS)
prop_namedPipePipelined_IO :: (Int -> Int -> (Int, Int)) -> [Int]
                           -> Property
prop_namedPipePipelined_IO f xs = ioProperty $ do
    let client = reqRespClientPeerPipelined (reqRespClientMapPipelined xs)
        server = reqRespServerPeer          (reqRespServerMapAccumL
                                                (\a -> pure . f a) 0)
    let cliPath = "client.sock"
        srvPath = "server.sock"
        mode = Posix.ownerModes

    Posix.createNamedPipe cliPath mode
    Posix.createNamedPipe srvPath mode

    bracket   (openFile cliPath ReadWriteMode)
              (\_ -> removeFile cliPath)
           $ \cliHandle ->
      bracket (openFile srvPath ReadWriteMode)
              (\_ -> removeFile srvPath)
           $ \srvHandle -> do
              (c, s) <- runConnectedPeersPipelined
                          (return ( handlesAsChannel cliHandle srvHandle
                                  , handlesAsChannel srvHandle cliHandle
                                  ))
                          nullTracer
                          CBOR.codecReqResp
                          client server
              return ((s, c) == mapAccumL f 0 xs)
#endif


#if !defined(mingw32_HOST_OS)
prop_socketPipelined_IO :: (Int -> Int -> (Int, Int)) -> [Int]
                        -> Property
prop_socketPipelined_IO f xs = ioProperty $ do
    ai : _ <- Socket.getAddrInfo (Just Socket.defaultHints
                                       { Socket.addrFamily     = Socket.AF_INET,
                                         Socket.addrFlags      = [Socket.AI_PASSIVE],
                                         Socket.addrSocketType = Socket.Stream })
                                 (Just "127.0.0.1") Nothing
    bracket
      ((,) <$> Socket.openSocket ai
           <*> Socket.openSocket ai)
      ( \ (sock, sock') -> Socket.close sock
                        >> Socket.close sock')
      $ \ (sock, sock') -> do
          Socket.bind sock (Socket.addrAddress ai)
          addr <- Socket.getSocketName sock
          Socket.listen sock 1
          Socket.connect sock' addr
          bracket (fst <$> Socket.accept sock) Socket.close
                $ \sock'' -> do
            let client = reqRespClientPeerPipelined (reqRespClientMapPipelined xs)
                server = reqRespServerPeer          (reqRespServerMapAccumL
                                                        (\a -> pure . f a) 0)

            (c, s) <- runConnectedPeersPipelined
                        (return ( socketAsChannel sock'
                                , socketAsChannel sock''
                                ))
                        nullTracer
                        CBOR.codecReqResp
                        client server
            return ((s, c) == mapAccumL f 0 xs)
#endif

--
-- Codec properties
--

instance (Arbitrary req, Arbitrary resp) =>
         Arbitrary (AnyMessage (ReqResp req resp)) where
  arbitrary = oneof
    [ AnyMessage . MsgReq <$> arbitrary
    , AnyMessage . MsgResp <$> arbitrary
    , return (AnyMessage MsgDone)
    ]

  shrink (AnyMessage (MsgReq r))  =
    [ AnyMessage (MsgReq r')
    | r' <- shrink r ]

  shrink (AnyMessage (MsgResp r)) =
    [ AnyMessage (MsgResp r')
    | r' <- shrink r ]

  shrink (AnyMessage MsgDone)     = []

instance (Eq req, Eq resp) => Eq (AnyMessage (ReqResp req resp)) where
  (AnyMessage (MsgReq  r1)) == (AnyMessage (MsgReq  r2)) = r1 == r2
  (AnyMessage (MsgResp r1)) == (AnyMessage (MsgResp r2)) = r1 == r2
  (AnyMessage MsgDone)      == (AnyMessage MsgDone)      = True
  _                         == _                         = False

prop_codec_ReqResp :: AnyMessage (ReqResp String String) -> Property
prop_codec_ReqResp =
    prop_codec
      runIdentity
      codecReqResp

prop_codec_splits2_ReqResp :: AnyMessage (ReqResp String String)
                           -> Property
prop_codec_splits2_ReqResp =
    prop_codec_splits
      splits2
      runIdentity
      codecReqResp

prop_codec_splits3_ReqResp :: AnyMessage (ReqResp String String)
                           -> Property
prop_codec_splits3_ReqResp =
    prop_codec_splits
      splits3
      runIdentity
      codecReqResp

prop_codec_cbor_ReqResp
  :: AnyMessage (ReqResp String String)
  -> Property
prop_codec_cbor_ReqResp msg =
  runST $ prop_codecM CBOR.codecReqResp msg

prop_codec_cbor_splits2_ReqResp
  :: AnyMessage (ReqResp String String)
  -> Property
prop_codec_cbor_splits2_ReqResp msg =
  runST $ prop_codec_splitsM
      splits2BS
      CBOR.codecReqResp
      msg

prop_codec_cbor_splits3_ReqResp
  :: AnyMessage (ReqResp String String)
  -> Property
prop_codec_cbor_splits3_ReqResp msg =
  runST $ prop_codec_splitsM
      splits3BS
      CBOR.codecReqResp
      msg


instance (Show a, Arbitrary a) => Arbitrary (WithBytes a) where
  arbitrary = mkWithBytes <$> arbitrary
  shrink WithBytes { message } = mkWithBytes <$> shrink message

prop_anncodec_ReqResp
  :: AnyMessage (ReqResp (WithBytes Int) (WithBytes Int))
  -> Property
prop_anncodec_ReqResp =
  prop_anncodec
    runIdentity
    anncodecReqResp

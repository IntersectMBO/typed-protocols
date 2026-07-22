{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Network.TypedProtocol.ReqResp.Server where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Server
import Network.TypedProtocol.ReqResp.Type


data ReqRespServer req resp m a = ReqRespServer {
    -- | The client sent us a ping message. We have no choices here, and
    -- the response is nullary, all we have are local effects.
    recvMsgReq  :: req -> m (resp, ReqRespServer req resp m a)

    -- | The client terminated. Here we have a pure return value, but we
    -- could have done another action in 'm' if we wanted to.
  , recvMsgDone :: m a
  }


-- | Interpret a particular server action sequence into the server side of the
-- 'ReqResp' protocol.
--
reqRespServerPeer
  :: Monad m
  => ReqRespServer req resp m a
  -> Server (ReqResp req resp) NonPipelined StIdle m a
reqRespServerPeer ReqRespServer{..} =

    -- In the 'StIdle' the server is awaiting a request message
    Await $ \msg ->

    -- The client got to choose between two messages and we have to handle
    -- either of them
    case msg of

      -- The client sent the done transition, so we're in the 'StDone' state
      -- so all we can do is stop using 'done', with a return value.
      MsgDone -> Effect $ Done <$> recvMsgDone

      -- The client sent us a ping request, so now we're in the 'StBusy' state
      -- which means it's the server's turn to send.
      MsgReq req -> Effect $ do
        (resp, next) <- recvMsgReq req
        pure $ Yield (MsgResp resp) (reqRespServerPeer next)


-- | An anti-pipelined 'ReqResp' server.
--
-- The single 'Sender' takes no argument, so a reply can depend on its request
-- only indirectly — by the peer stashing request-derived data into state (via
-- an 'Effect') for the 'Sender' to read. This example doesn't do that: it
-- ignores the request payload (hence @()@) and draws each reply from the
-- supplied action, which typically reads and advances some state. It receives
-- ahead, delegating every reply to the 'Sender', and is willing — at the
-- environment's choice — either to collect an outstanding reply or keep
-- receiving.
--
reqRespServerPeerDualPipelined1
  :: forall resp m. Functor m
  => m resp
  -- ^ produce (and record) the next reply
  -> ServerDualPipelined1 (ReqResp () resp) StIdle m ()
reqRespServerPeerDualPipelined1 nextResp =
    ServerDualPipelined1 sender (go Zero)
  where
    sender :: Sender (ReqResp () resp) StBusy StIdle m
    sender = SenderEffect $
      (\resp -> SenderYield (MsgResp resp) SenderDone) <$> nextResp

    -- with @n@ replies outstanding: collect one if the environment chooses,
    -- but stay willing to receive ahead
    go :: forall n.
          Nat n
       -> Server (ReqResp () resp) (DualPipelined1 StBusy StIdle n) StIdle m ()
    go  Zero     = await Zero
    go (Succ n') = DualCollect1 (await n') (Just (await (Succ n')))

    await :: forall n.
             Nat n
          -> Server (ReqResp () resp) (DualPipelined1 StBusy StIdle n) StIdle m ()
    await n = Await $ \msg -> case msg of
                MsgReq _ -> YieldDualPipelined1 (go (Succ n))
                MsgDone  -> drain n

    drain :: forall n.
             Nat n
          -> Server (ReqResp () resp) (DualPipelined1 StBusy StIdle n) StDone m ()
    drain  Zero     = Done ()
    drain (Succ n') = DualCollect1 (drain n') Nothing

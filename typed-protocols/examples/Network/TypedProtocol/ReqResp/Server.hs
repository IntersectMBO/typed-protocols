{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Network.TypedProtocol.ReqResp.Server where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Server
import Network.TypedProtocol.Proofs (embedLookaheadUsingFixedSender)
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


-- | A lookahead 'ReqResp' server (the dual of a pipelined client).
--
-- It is exactly 'reqRespServerPeerLookaheadFixedSender' with its single
-- 'Sender' plugged in at each 'AwaitLookahead' (via
-- 'embedLookaheadUsingFixedSender'), so the two never drift apart.
--
reqRespServerPeerLookahead
  :: forall resp m. Functor m
  => m resp
  -- ^ produce (and record) the next reply
  -> ServerLookahead (ReqResp () resp) StIdle m ()
reqRespServerPeerLookahead =
    embedLookaheadUsingFixedSender . reqRespServerPeerLookaheadFixedSender


-- | A lookahead 'ReqResp' server that receives requests ahead of sending their
-- replies: each 'AwaitLookahead' hands the reply to the /previous/ request off
-- to the sender thread (via 'TheSender', the one 'Sender' carried by the
-- 'ServerLookaheadFixedSender' wrapper — it runs @nextResp@, which typically
-- reads and advances some state), while immediately awaiting the next request.
-- The request payload is ignored (hence @()@); replies come solely from
-- @nextResp@.  This is the peer that
-- 'Network.TypedProtocol.Driver.runLookaheadFixedSenderPeerWithDriver' runs.
--
reqRespServerPeerLookaheadFixedSender
  :: forall resp m. Functor m
  => m resp
  -- ^ produce (and record) the next reply
  -> ServerLookaheadFixedSender (ReqResp () resp) StIdle m ()
reqRespServerPeerLookaheadFixedSender nextResp =
    ServerLookaheadFixedSender sender start
  where
    sender :: Sender (ReqResp () resp) VariableSender StBusy StIdle m
    sender = SenderEffect $
      (\resp -> SenderYield (MsgResp resp) SenderDone) <$> nextResp

    start :: Server (ReqResp () resp) (Lookahead Z (FixedSender StBusy StIdle)) StIdle m ()
    start = Await $ \msg -> case msg of
              MsgReq _ -> busy Zero
              MsgDone  -> Done ()

    busy :: forall n.
            Nat n
         -> Server (ReqResp () resp) (Lookahead n (FixedSender StBusy StIdle)) StBusy m ()
    busy n = AwaitLookahead TheSender $ \msg -> case msg of
               MsgReq _ -> busy  (Succ n)
               MsgDone  -> drain (Succ n)

    drain :: forall n.
             Nat n
          -> Server (ReqResp () resp) (Lookahead n (FixedSender StBusy StIdle)) StDone m ()
    drain  Zero     = Done ()
    drain (Succ n') = FlushSender Nothing (drain n')

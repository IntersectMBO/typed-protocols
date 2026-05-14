{-# LANGUAGE TypeFamilies #-}

-- | An RPC protocol which in which request type determines respond time.
-- Unlike in the `Network.TypedProtocol.ReqResp.Type` where `req` and `resp`
-- types where statically defined, here the respond type is dynamically
-- determined by the type of request.
--
module Network.TypedProtocol.Stateful.ReqResp.Type where

import Data.Kind (Type)
import Data.Typeable
import Network.TypedProtocol.Core


type ReqResp :: (Type -> Type) -> Type
data ReqResp req where
  StIdle :: ReqResp req
  StBusy :: res
         -> ReqResp req
  StDone :: ReqResp req

type SReqResp :: ReqResp req -> Type
data SReqResp st where
  SingIdle :: SReqResp StIdle
  SingBusy :: SReqResp (StBusy res :: ReqResp req)
  SingDone :: SReqResp StDone

deriving instance Show (SReqResp st)

instance StateTokenI StIdle       where stateToken = SingIdle
instance StateTokenI (StBusy res) where stateToken = SingBusy
instance StateTokenI StDone       where stateToken = SingDone


instance Protocol (ReqResp req) where

  -- Messages for the `ReqResp` protocol.
  --
  -- Typeable constraint is used to support
  -- `Network.TypeProtocol.Stateful.ReqResp.Codec.codecReqRespId' - an
  -- efficient encoder / decoder useful for testing purposes.
  --
  data Message (ReqResp req) from to where
    MsgReq  :: Typeable resp
            => req resp -- ^ request which expects `resp` as a result, `resp` is
                        --   promoted to the state `StBusy` state.
            -> Message (ReqResp req) StIdle (StBusy resp)
    MsgResp :: Typeable resp
            => resp     -- ^ respond
            -> Message (ReqResp req) (StBusy resp) StIdle
    MsgDone :: Message (ReqResp req) StIdle StDone

  type StateAgency StIdle     = ClientAgency
  type StateAgency (StBusy _) = ServerAgency
  type StateAgency StDone     = NobodyAgency

  type StateToken = SReqResp


-- deriving instance Show req
--                => Show (Message (ReqResp req) from to)
--
-- deriving instance Eq req
--                => Eq (Message (ReqResp req) from to)

type State :: ReqResp req -> Type
data State st where
  StateIdle :: State StIdle
  -- fancy type signature is needed to help GHC infer that when pattern
  -- matching on  `StateBusy resp` then `resp :: Type`
  StateBusy :: forall (req :: Type -> Type)
                      (result ::  Type).
               Typeable result
            => req result
            -> State (StBusy result :: ReqResp req)
  StateDone :: State StDone

--
-- A simple example RPC
--

-- | An example RPC, e.g. the `req` type.
--
type FileAPI :: Type -> Type
data FileAPI result where
  ReadFile  :: FilePath -> FileAPI String
  -- read a file

  WriteFile :: FilePath -> String -> FileAPI ()
  -- write to a file
-- TODO: intersectmbo/typed-protocols#57

type FileRPC = ReqResp FileAPI

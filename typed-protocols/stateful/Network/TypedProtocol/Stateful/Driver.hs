{-# LANGUAGE CPP #-}

-- | Actions for running 'Peer's with a 'Driver'.  This module should be
-- imported qualified.
--
module Network.TypedProtocol.Stateful.Driver
  ( -- * DriverIngerface
    Driver (..)
    -- * Running a peer
  , runPeerWithDriver
    -- * Re-exports
  , SomeMessage (..)
  , DecodeStep (..)
  ) where

import Control.DeepSeq (NFData, force)
#if !MIN_VERSION_io_classes(1,10,0)
import Control.Monad.Class.MonadSTM
#endif
import Control.Monad.Class.MonadThrow

import Data.Kind (Type)

import Network.TypedProtocol.Codec (DecodeStep (..), SomeMessage (..))
import Network.TypedProtocol.Core
import Network.TypedProtocol.Stateful.Peer

data Driver ps (pr :: PeerRole) bytes failure dstate f m =
        Driver {
          -- | Send a message.
          --
          sendMessage   :: forall (st :: ps) (st' :: ps).
                           StateTokenI st
                        => StateTokenI st'
                        => ActiveState st
                        => ReflRelativeAgency (StateAgency st)
                                               WeHaveAgency
                                              (Relative pr (StateAgency st))
                        -> f st
                        -- local state associated to protocol state `st`;
                        -- local state should not be sent to the remote side.
                        -- However it provide extra context for the encoder.
                        --
                        -- TODO: intersectmbo/typed-protocols#57
                        -> Message ps st st'
                        -- message to send
                        --
                        -- TODO: intersectmbo/typed-protocols#57
                        -> m ()

        , -- | Receive a message, a blocking action which reads from the network
          -- and runs the incremental decoder until a full message is decoded.
          --
          recvMessage   :: forall (st :: ps).
                           StateTokenI st
                        => ActiveState st
                        => ReflRelativeAgency (StateAgency st)
                                               TheyHaveAgency
                                              (Relative pr (StateAgency st))
                        -> f st
                        -- local state which provides extra context for the
                        -- decoder.
                        --
                        -- TODO: intersectmbo/typed-protocols#57
                        -> dstate
                        -- decoder state, e.g. bytes left from decoding of
                        -- a previous message.
                        --
                        -- TODO: intersectmbo/typed-protocols#57
                        -> m (SomeMessage st, dstate)

        , -- | Initial decoder state.
          --
          initialDState :: dstate
        }


--
-- Running peers
--

-- | Run a peer with the given driver.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
-- NOTE: this function threads local state (i.e. `f`) through evolution of
-- a protocol (i.e. `Peer`).
--
runPeerWithDriver
  :: forall ps (st :: ps) pr bytes failure dstate (f :: ps -> Type) m a.
     ( MonadEvaluate m
#if !MIN_VERSION_io_classes(1,10,0)
     , MonadSTM m
#endif
     , NFData a
     )
  => Driver ps pr bytes failure dstate f m
  -> f st
  -> Peer ps pr st f m a
  -> m (a, dstate)
runPeerWithDriver Driver{ sendMessage
                        , recvMessage
                        , initialDState
                        } =
    go initialDState
  where
    go :: forall st'.
          dstate
       -> f st'
       -> Peer ps pr st' f m a
       -> m (a, dstate)
    go !dstate !f (Effect k) = k >>= go dstate f

    go !dstate  _ (Done _ x) = do
      x' <- evaluate (force x)
      return (x', dstate)

    go !dstate  _ (Yield refl !f !f' msg k) = do
      sendMessage refl f msg
      go dstate f' k

    go !dstate !f (Await refl k) = do
      (SomeMessage msg, dstate') <- recvMessage refl f dstate
      case k f msg of
        (k', f') -> go dstate' f' k'

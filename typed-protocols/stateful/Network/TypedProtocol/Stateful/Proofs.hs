{-# LANGUAGE TypeFamilies #-}

-- This is already implied by the -Wall in the .cabal file, but lets just be
-- completely explicit about it too, since we rely on the completeness
-- checking in the cases below for the completeness of our proofs.
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Proofs about the typed protocol framework.
--
-- It also provides helpful testing utilities.
--
module Network.TypedProtocol.Stateful.Proofs
  ( connect
  , TerminalStates (..)
  , removeState
  ) where

import Control.Monad.Class.MonadSTM

import Data.Kind (Type)
import Data.Singletons

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer
import Network.TypedProtocol.Proofs (TerminalStates (..))
import Network.TypedProtocol.Proofs qualified as TP
import Network.TypedProtocol.Stateful.Peer qualified as ST



-- | Remove state for non-pipelined peers.
--
-- TODO: There's a difficulty to write `removeState` for pipelined peers which
-- is type safe.  The `Peer` doesn't track all pipelined transitions, just the
-- depth of pipelining, so we cannot push `f st` to a queue which type is
-- linked to `Peer`.  For a similar reason there's no way to write
-- `forgetPipelined` function.
--
-- However, this is possible if `Peer` tracks all transitions.
--
removeState
  :: Functor m
  => f st
  -> ST.Peer ps pr              st f m a
  ->    Peer ps pr NonPipelined st   m a
removeState = go
  where
    go
      :: forall ps (pr :: PeerRole)
                (st :: ps)
                (f :: ps -> Type)
                m a.
         Functor m
      => f st
      -> ST.Peer ps pr              st f m a
      ->    Peer ps pr NonPipelined st   m a
    go f (ST.Effect k) = Effect (go f <$> k)
    go _ (ST.Yield refl _f f' msg k) = Yield refl msg (go f' k)
    go f (ST.Await refl k) = Await refl $ \msg ->
      case k f msg of
        (k', f') -> go f' k'
    go _ (ST.Done refl a) = Done refl a


connect
  :: forall ps (pr :: PeerRole)
               (st :: ps)
               (f :: ps -> Type)
               m a b.
       (MonadSTM m, SingI pr)
    => f st
    -> ST.Peer ps             pr  st f m a
    -> ST.Peer ps (FlipAgency pr) st f m b
    -> m (a, b, TerminalStates ps)
connect f a b = TP.connect (removeState f a) (removeState f b)

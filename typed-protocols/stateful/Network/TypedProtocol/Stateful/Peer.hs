-- TODO: the 'Functor' instance of 'Peer' is undecidable
{-# LANGUAGE UndecidableInstances #-}

-- | Protocol stateful EDSL.
--
-- __Note__: 'Network.TypedProtocol.Peer.Client.Client' and
-- 'Network.TypedProtocol.Peer.Server.Server' patterns are easier to use.
--
module Network.TypedProtocol.Stateful.Peer (Peer (..)) where

import Data.Kind (Type)

import Network.TypedProtocol.Core as Core


-- | A description of a peer that engages in a protocol.
--
-- The protocol describes what messages peers /may/ send or /must/ accept.
-- A particular peer implementation decides what to actually do within the
-- constraints of the protocol.
--
-- Peers engage in a protocol in either the client or server role. Of course
-- the client role can only interact with the serve role for the same protocol
-- and vice versa.
--
-- 'Peer' has several type arguments:
--
-- * the protocol itself;
-- * the client\/server role;
-- * the current protocol state;
-- * the local state type;
-- * the monad in which the peer operates; and
-- * the type of any final result once the peer terminates.
--
-- For example:
--
-- > reqRespClientExample :: Peer (ReqResp FileAPI) AsClient StIdle State m ()
-- > reqRespServerExample :: Peer (ReqResp FileAPI) AsServer StIdle State m Int
--
-- The actions that a peer can take are:
--
-- * perform a local monadic effect,
-- * terminate with a result (but only in a terminal protocol state),
-- * send a message (but only in a protocol state in which we have agency),
-- * wait to receive a message (but only in a protocol state in which the
--   other peer has agency).
--
-- The 'Yield', 'Await' and 'Done' constructors require to provide an evidence
-- that the appropriate peer has agency.  This information is supplied using
-- one of the constructors of 'ReflRelativeAgency'.
--
-- While this evidence must be provided, the types guarantee that it is not
-- possible to supply incorrect evidence.  The
-- 'Network.TypedProtocol.Peer.Client' or 'Network.TypedProtocol.Peer.Server'
-- pattern synonyms provide this evidence automatically.
--
-- __NOTE__:
-- one should not allocate resources within a `Peer` since there's no way to
-- safely clean-up resources in case of an asynchronous exception.  The proper
-- way to allocate resources is to call
-- `Network.TypedProtocol.Stateful.Driver.runPeerWithDriver` within
-- a `Control.Exception.bracket`.
--
-- __TODO__:
-- We are not exposing pipelined version, since it is not possible to write
-- a driver & proofs in a type safe which take into account the state when the
-- peer type only tracks depth of pipelining rather than pipelined transitions.
--
type Peer :: forall ps
          -> PeerRole
          -> ps
          -> (ps -> Type)
          -- ^ protocol state
          -> (Type -> Type)
          -- ^ monad's kind
          -> Type
          -> Type
data Peer ps pr st f m a where

  -- | Perform a local monadic effect and then continue.
  --
  -- Example:
  --
  -- > Effect $ do
  -- >   ...          -- actions in the monad
  -- >   return $ ... -- another Peer value
  --
  Effect
    :: forall ps pr st f m a.
       m (Peer ps pr st f m a)
    -- ^ monadic continuation
    ->    Peer ps pr st f m a

  -- | Send a message to the other peer and then continue. The constructor
  -- requires evidence that we have agency for this protocol state and thus are
  -- allowed to send messages.  It takes local state associated to the source
  -- and target protocol state of the message that is sent.  This state is only
  -- maintained locally, never shared remotely.  It also takes the message and
  -- the continuation. It also requires evidence that we have agency for this
  -- protocol state and thus are allowed to send messages.
  --
  -- Example:
  --
  -- > Yield ReflClientAgency (StateBusy (ReadFile /etc/os-release))
  -- >                        StateIdle
  -- >                      $ MsgResp "..."
  --
  Yield
    :: forall ps pr (st :: ps) (st' :: ps) f m a.
       ( StateTokenI st
       , StateTokenI st'
       , ActiveState st
       )
    => WeHaveAgencyProof pr st
    -- ^ agency singleton
    -> f st
    -- ^ associated local state to the source protocol state 'st'
    -> f st'
    -- ^ associated local state to the target protocol state `st'`
    -> Message ps st st'
    -- ^ protocol message
    -> Peer ps pr st' f m a
    -- ^ continuation
    -> Peer ps pr st  f m a

  -- | Waits to receive a message from the other peer and then continues.
  -- This takes the continuation that is supplied with the received message. It
  -- also requires evidence that the other peer has agency for this protocol
  -- state and thus we are expected to wait to receive messages.
  --
  -- Note that the continuation that gets supplied with the message must be
  -- prepared to deal with /any/ message that is allowed in /this/ protocol
  -- state. This is why the continuation /must/ be polymorphic in the target
  -- state of the message (the third type argument of 'Message').
  --
  -- Example:
  --
  -- > Await ReflClientAgency $ \f msg ->
  -- > case (f, msg) of
  -- >   (StateBusy (ReadFile path), MsgResp resp) ->
  -- >     ( _continuation
  -- >     , StateIdle
  -- >     )
  --
  --
  Await
    :: forall ps pr (st :: ps) f m a.
       ( StateTokenI st
       , ActiveState st
       )
    => TheyHaveAgencyProof pr st
    -- ^ agency singleton
    -> (forall (st' :: ps).
           f st
        -- associated local state to the source protocol state 'st'
        --
        -- TODO: intersectmbo/typed-protocols#57
        -> Message ps st st'
        -> ( Peer ps pr st' f m a
           , f st'
           )
         -- continuation and associated local state to the target protocol
         -- state `st'`
         --
         -- NOTE: the API is limited to pure transition of local state e.g.
         -- `f st -> Message ps st st' -> f st'`,
         -- see https://github.com/intersectmbo/typed-protocols/discussions/63
         --
         -- TODO: intersectmbo/typed-protocols#57
       )
    -- ^ continuation
    -> Peer ps pr st f m a

  -- | Terminate with a result. A state token must be provided from the
  -- 'NobodyHasAgency' states, to show that this is a state in which we can
  -- terminate.
  --
  -- Example:
  --
  -- > Yield ReflClientAgency
  -- >        MsgDone
  -- >       (Done ReflNobodyAgency TokDone result)
  --
  Done
    :: forall ps pr (st :: ps) f m a.
       ( StateTokenI st
       , StateAgency st ~ NobodyAgency
       )
    => NobodyHasAgencyProof pr st
    -- ^ (no) agency proof
    -> a
    -- ^ returned value
    -> Peer ps pr st f m a

deriving instance Functor m => Functor (Peer ps pr st f m)

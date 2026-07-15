{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ViewPatterns    #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- | This module defines the core of the typed protocol framework.
--
module Network.TypedProtocol.Core
  ( -- * Introduction
    -- $intro

    -- * Defining protocols
    -- $defining
    Protocol (..)
  , StateTokenI (..)
    -- $lemmas

    -- * Engaging in protocols
    -- ** PeerRole
  , PeerRole (..)
  , SingPeerRole (..)
    -- ** Agency and its evidence
    -- $agency
  , Agency (..)
  , SingAgency (..)
  , RelativeAgency (..)
  , Relative
  , ReflRelativeAgency (..)
  , WeHaveAgencyProof
  , TheyHaveAgencyProof
  , NobodyHasAgencyProof
    -- *** FlipAgency
  , FlipAgency
    -- *** ActiveState
  , IsActiveState (..)
  , ActiveState
  , notActiveState
  , ActiveAgency
  , ActiveAgency' (..)
    -- ** Pipelining
    -- *** IsPipelined
  , IsPipelined (..)
    -- *** Outstanding
  , Outstanding
  , AntiOutstanding
    -- *** N and Nat
  , N (..)
  , Nat (Succ, Zero)
  , natToInt
  , unsafeIntToNat
  ) where

import Data.Kind (Constraint, Type)
import Unsafe.Coerce (unsafeCoerce)

import Data.Singletons

-- $intro
-- A typed protocol between two peers is defined via a state machine: a
-- collection of protocol states and protocol messages which are transitions
-- between those states.
--
-- Start from the idea that a protocol is some language of messages sent
-- between two peers. To specify a protocol is to describe what possible
-- sequences of messages are valid. One simple but still relatively expressive
-- way to do this is via a state machine: starting from some initial state,
-- all possible paths through the state machine gives the set of valid protocol
-- traces. This then dictates what a peer participating in a protocol may
-- produce and what it must accept.
--
-- In this style we have a fixed number of states and in each state there is
-- some number of valid messages that move us on to the next state. This can be
-- illustrated as a graph, which can be a helpful form of documentation.
--
-- We further constrain this idea by saying that the two peers will use the
-- same state machine and change states in lock-step by sending\/receiving
-- messages. In this approach, for each protocol state, the description
-- dictates which peer has the agency to choose to send a message, while
-- correspondingly the other must be prepared to receive the message.
--
-- The views of the two peers are dual. In each state one peer can send any
-- message that is valid for the current protocol state while the other
-- must be prepared to receive any valid message for current protocol state.
--
-- We can also have terminal protocol states in which neither peer has agency.
--
-- So part of the protocol description is to label each protocol state with
-- the peer that has the agency in that state, or none for terminal states.
-- We use the labels \"client\" and \"server\" for the two peers, but they are
-- in fact symmetric.


-- $defining
--
-- The 'Protocol' type class bundles up all the requirements for a typed
-- protocol, which are in fact all type level constructs. Defining a new
-- protocol and making it an instance of the 'Protocol' class requires the
-- following language extensions:
--
-- > {-# LANGUAGE GADTs, TypeFamilies, DataKinds #-}
--
-- The type class itself is indexed on a protocol \"tag\" type. This type
-- does double duty as the /kind/ of the /types/ of the protocol states.
--
-- We will use as a running example a simple \"ping\/pong\" protocol. (You can
-- see the example in full in "Network.TypedProtocol.PingPong.Type".) In this
-- example protocol the client sends a ping message and the serve must respond
-- with a pong message. The client can also terminate the protocol. So modelled
-- as a state machine this protocol has three states, the one in which the
-- client can send a ping or terminate message, the one in which the server
-- must send a pong, and the terminal state where neither can send anything.
-- We somewhat arbitrarily choose label these protocol states as \"idle\"
-- \"busy\" and \"done\".
--
-- For this ping pong example the protocol tag and the protocol state types
-- would be defined (via promoted data kinds) as:
--
-- > data PingPong where
-- >   StIdle :: PingPong
-- >   StBusy :: PingPong
-- >   StDone :: PingPong
--
-- We use @DataKinds@ promotion here so @StIdle@, @StBusy@ and @StDone@ are
-- /types/ (of /kind/ @PingPong@) representing the three states in this
-- protocol's state machine. @PingPong@ itself is both the kind of these types
-- and is also the tag for protocol. We only ever use these as types, via the
-- @DataKinds@ promotion, never as value level data constructors.
--
-- Having defined our protocol tag and states we can instantiate the 'Protocol'
-- type class and fill out the other details.
--
-- The protocol must define what its messages are. These form the state
-- transitions in the protocol state machine. Each transition specifies a
-- \"from\" and \"to\" state as type parameters. This of course determines in
-- which protocol states each message can appear.
--
-- In the \"ping\/pong\" protocol example, the messages are of course ping and
-- pong, which transition between the two main states. There is also a done
-- message that moves the system into a terminal state.
--
-- > instance Protocol PingPong where
-- >   data Message PingPong from to where
-- >     MsgPing :: Message PingPong StIdle StBusy
-- >     MsgPong :: Message PingPong StBusy StIdle
-- >     MsgDone :: Message PingPong StIdle StDone
--
-- This says that in the idle state a ping message takes us to the busy state,
-- while a pong message takes us back to idle. Also in the idle state a done
-- message takes us to the done state.
--
-- It is not required that protocols have any terminal states or corresponding
-- transitions, as in this example, but it is often useful and it aids testing
-- to have protocols that terminate cleanly as it allows them to return a
-- result.
--
-- As described above, this style of protocol gives agency to only one peer at
-- once. That is, in each protocol state, one peer has agency (the ability to
-- send) and the other does not (it must only receive).
--
-- In the \"ping\/pong\" protocol example, the idle state is the one in which
-- the client can send a message, and the busy state is the one in which the
-- server must respond. Finally in the done state, neither peer can send any
-- further messages. This arrangement is defined as so:
--
-- >    -- still within the instance Protocol PingPong
-- >    type StateAgency StIdle = ClientAgency
-- >    type StateAgency StBusy = ServerAgency
-- >    type StateAgency StDone = NobodyAgency
--
-- In this simple protocol there is exactly one state in each category, but in
-- general for non-trivial protocols there may be several protocol states in
-- each category.
--
-- Finally we need to point which singletons to use for the protocol states
--
-- >    -- still within the instance Protocol PingPong, 'SPingPong' type is what we define next.
-- >    type StateToken = SPingPong
--
-- Furthermore we use singletons to provide term level reflection of type level
-- states.  One is required to provide singletons for all types of kind
-- 'PingPong'.  These definitions are provided outside of the 'Protocol' type
-- class.  This is as simple as providing a GADT:
--
-- > data SingPingPong (st :: PingPong) where
-- >   SingIdle :: SingPingPong StIdle
-- >   SingBusy :: SingPingPong StBusy
-- >   SingDone :: SingPingPong StDone
--
-- together with 'StateTokenI' instance (similar to 'SingI' from the
-- "singletons" package):
--
-- > instance StateTokenI StIdle where stateToken = SingIdle
-- > instance StateTokenI StBusy where stateToken = SingBusy
-- > instance StateTokenI StDone where stateToken = SingDone
--
-- This and other example protocols are provided in "typed-protocols-examples"
-- package.

-- | Types for client and server peer roles. As protocol can be viewed from
-- either client or server side.
--
-- Note that technically \"client\" and \"server\" are arbitrary labels. The
-- framework is completely symmetric between the two peers.
--
-- This definition is only used as promoted types and kinds, never as values.
--
data PeerRole = AsClient | AsServer

-- | Singletons for 'PeerRole'.  We provide 'Sing' and 'SingI' instances from
-- the "singletons" package.
--
type SingPeerRole :: PeerRole -> Type
data SingPeerRole pr where
    SingAsClient :: SingPeerRole AsClient
    SingAsServer :: SingPeerRole AsServer

deriving instance Show (SingPeerRole pr)

type instance Sing = SingPeerRole
instance SingI AsClient where
    sing = SingAsClient
instance SingI AsServer where
    sing = SingAsServer

-- $agency
-- The protocols we consider either give agency to one side (one side can send
-- a message) or the protocol terminated.  Agency is a (type-level) function of
-- the protocol state, and thus uniquely determined by it.
--
-- The following types define the necessary type-level machinery and its
-- term-level evidence to provide type-safe API for `typed-protocols`.
-- Required proofs are hidden in an (unexposed) module
-- @Network.TypedProtocol.Lemmas@.

-- | A promoted data type which denotes three possible agencies a protocol
-- state might be assigned.
--
data Agency where
    -- | The client has agency.
    ClientAgency :: Agency

    -- | The server has agency.
    ServerAgency :: Agency

    -- | Nobody has agency, terminal state.
    NobodyAgency :: Agency

type SingAgency :: Agency -> Type
data SingAgency a where
    SingClientAgency :: SingAgency ClientAgency
    SingServerAgency :: SingAgency ServerAgency
    SingNobodyAgency :: SingAgency NobodyAgency

deriving instance Show (SingAgency a)

type instance Sing = SingAgency
instance SingI ClientAgency where
    sing = SingClientAgency
instance SingI ServerAgency where
    sing = SingServerAgency
instance SingI NobodyAgency where
    sing = SingNobodyAgency

-- | A promoted data type which indicates the effective agency (which is
-- relative to current role).  It is computed by `Relative` type family.
--
data RelativeAgency where
    -- evidence that we have agency
    WeHaveAgency    :: RelativeAgency
    -- evidence that proof the remote side has agency
    TheyHaveAgency  :: RelativeAgency
    -- evidence of protocol termination
    NobodyHasAgency :: RelativeAgency
-- TODO: input-output-hk/typed-protocols#57


-- | Compute effective agency with respect to the peer role, for client role,
-- agency is preserved, while for the server role it is flipped.
--
type        Relative :: PeerRole -> Agency -> RelativeAgency
type family Relative  pr a where
  Relative AsClient ClientAgency = WeHaveAgency
  Relative AsClient ServerAgency = TheyHaveAgency
  Relative AsClient NobodyAgency = NobodyHasAgency
  Relative AsServer ClientAgency = TheyHaveAgency
  Relative AsServer ServerAgency = WeHaveAgency
  Relative AsServer NobodyAgency = NobodyHasAgency


-- | Type equality for 'RelativeAgency' which also carries information about
-- agency.  It is isomorphic to a product of 'Agency' singleton and
-- @r :~: r'@, where both @r@ and @r'@ have kind 'RelativeAgency'.
--
-- This is a proper type with values used by the 'Peer', however they are
-- hidden by using "Network.TypedProtocol.Peer.Client" and
-- "Network.TypedProtocol.Peer.Server" APIs.
--
type ReflRelativeAgency :: Agency -> RelativeAgency -> RelativeAgency -> Type
data ReflRelativeAgency a r r' where
    ReflClientAgency :: ReflRelativeAgency ClientAgency r r
    ReflServerAgency :: ReflRelativeAgency ServerAgency r r
    ReflNobodyAgency :: ReflRelativeAgency NobodyAgency r r

-- | Type of the proof that we have the agency.
--
-- 'ReflClientAgency' has this type only iff `'StateAgency' st ~ 'ClientAgency'`
-- and `pr ~ 'AsClient'`.
--
-- 'ReflServerAgency' has this type only iff `'StateAgency' st ~ 'ServerAgency'`
-- and `pr ~ 'AsServer'`
--
type WeHaveAgencyProof :: PeerRole -> ps -> Type
type WeHaveAgencyProof pr st = ReflRelativeAgency
                                 (StateAgency st)
                                  WeHaveAgency
                                 (Relative pr (StateAgency st))

-- | Type of the proof that the remote side has the agency.
--
-- 'ReflClientAgency' has this type only iff `'StateAgency' st ~ 'ClientAgency'`
-- and `pr ~ 'AsServer'`.
--
-- 'ReflServerAgency' has this type only iff `'StateAgency' st ~ 'ServerAgency'`
-- and `pr ~ 'AsClient'`
--
type TheyHaveAgencyProof :: PeerRole -> ps -> Type
type TheyHaveAgencyProof pr st = ReflRelativeAgency
                                   (StateAgency st)
                                    TheyHaveAgency
                                   (Relative pr (StateAgency st))


-- | Type of the proof that nobody has agency in this state.
--
-- Only 'ReflNobodyAgency' can fulfil the proof obligation.
--
type NobodyHasAgencyProof :: PeerRole -> ps -> Type
type NobodyHasAgencyProof pr st = ReflRelativeAgency (StateAgency st)
                                                      NobodyHasAgency
                                                     (Relative pr (StateAgency st))

-- $lemmas
--
-- The 'Network.TypedProtocol.connect' proof rely on lemmas about the
-- protocol. Specifically they rely on the property that each protocol state is
-- labelled with the agency of one peer or the other, or neither, but never
-- both.  This property is true by construction, since we use a type family
-- 'StateAgency' which maps states to agencies, however we still need an evince
-- that cases where both peer have the agency or neither of them has it can be
-- eliminated.
--
-- The packages defines lemmas (in a hidden module) which are structured as
-- proofs by contradiction, e.g. stating \"if the client and the server have
-- agency for this state then it leads to contradiction\". Contradiction is
-- represented as the 'Void' type that has no values except ⊥.
--
-- The framework defines protocol-agnostic proofs (in the hidden module
-- `Network.TypedProtocol.Lemmas`) which excludes that the client and server
-- have agency at the same time.
--
-- * 'exclusionLemma_ClientAndServerHaveAgency',
-- * 'terminationLemma_1',
-- * 'terminationLemma_2'.
--

-- | A type class which hides a state token / singleton inside a class
-- dictionary.
--
-- This is similar to the 'SingI' instance, but specific to protocol state
-- singletons.
--
class StateTokenI st where
    stateToken :: StateToken st

-- | The protocol type class bundles up all the requirements for a typed
-- protocol.
--
-- Each protocol consists of four components:
--
-- * the protocol itself, which is also expected to be the kind of the types
--   of the protocol states. The class is indexed on the protocol itself;
-- * the protocol messages;
-- * a type level map from the protocol states to agency: in each state either
--   client or server or nobody has the agency.
-- * a singleton type for the protocol states (e.g. `StateToken` type family
--   instance), together with 'StateTokenI' instances.
--
-- It is required provide 'StateToken' type family instance as well as
-- 'StateTokenI' instances for all protocol states.  These singletons allow one
-- to pattern match on the state, which is useful when defining codecs, or
-- providing informative error messages, however they are not necessary for
-- proving correctness of the protocol.  These type families are similar to
-- 'Sing' and 'SingI' in the "singletons" package.
--
class Protocol ps where

  -- | The messages for this protocol. It is expected to be a GADT that is
  -- indexed by the @from@ and @to@ protocol states. That is the protocol state
  -- the message transitions from, and the protocol state it transitions into.
  -- These are the edges of the protocol state transition system.
  --
  data Message ps (st :: ps) (st' :: ps)

  -- | Associate an 'Agency' for each state.
  --
  type StateAgency (st :: ps) :: Agency

  -- | A type family for protocol state token, e.g. term level representation of
  -- type level state (also known as singleton).
  --
  -- This type family is similar to 'Sing' type class in the "singletons"
  -- package, but specific for protocol states.
  --
  type StateToken :: ps -> Type


-- | Evidence that one side of the protocol has the agency, and thus that the
-- protocol hasn't yet terminated.
--
type ActiveAgency' :: ps -> Agency -> Type
data ActiveAgency' st agency where
  -- | Evidence that the client has the agency.
  ClientHasAgency :: StateAgency st ~ ClientAgency
                  => ActiveAgency' st ClientAgency
  -- | Evidence that the server has the agency.
  ServerHasAgency :: StateAgency st ~ ServerAgency
                  => ActiveAgency' st ServerAgency

deriving instance Show (ActiveAgency' st agency)

-- | Evidence that the protocol isn't in a terminal state.
--
type ActiveAgency :: ps -> Type
type ActiveAgency st = ActiveAgency' st (StateAgency st)


-- | A type class which restricts states to ones that have `ClientAgency` or
-- `ServerAgency`, excluding `NobodyAgency`.
--
-- One can use `notActive' to eliminate cases for which both @'IsActiveState'
-- st@ is in scope and for which we have an evidence that the state is not
-- active (i.e. a singleton).  This is useful when writing a 'Codec'.
--
class IsActiveState st (agency :: Agency) where
  activeAgency :: ActiveAgency' st agency

instance ClientAgency ~ StateAgency st
      => IsActiveState st ClientAgency where
  activeAgency = ClientHasAgency
instance ServerAgency ~ StateAgency st
      => IsActiveState st ServerAgency where
  activeAgency = ServerHasAgency

-- | A constraint which provides an evidence that the protocol isn't in
-- a terminal state.
--
type ActiveState :: ps -> Constraint
type ActiveState st = IsActiveState st (StateAgency st)


-- | This is useful function to eliminate cases where the `ActiveState st` is
-- provided but we are given a state in which neither side has agency
-- (`NobodyAgency`).  For example when writing a codec, we only need to encode
-- / decode messages which are in active states, but to make such functions
-- total, `notActiveState` needs to be used to eliminate the states in which
-- nobody has agency.
--
-- A good analogy for this function is @'Data.Void.absurd' :: 'Void' -> a@.
--
notActiveState :: forall ps (st :: ps).
                  StateAgency st ~ NobodyAgency
               => ActiveState st
               => StateToken st
               -> forall a. a
notActiveState (_ :: StateToken st) =
  case activeAgency :: ActiveAgency st of {}


-- | A type function to flip the client and server roles.
--
type        FlipAgency :: PeerRole -> PeerRole
type family FlipAgency pr where
  FlipAgency AsClient = AsServer
  FlipAgency AsServer = AsClient


-- | A type level inductive natural number.
data N = Z | S N

-- | Promoted data type which indicates if 'Peer' is used in
-- pipelined mode or not.
--
data IsPipelined ps where
    -- | Pipelined peer which is using `c :: Type` for collecting responses
    -- from a pipelined messages. 'N' indicates depth of pipelining.
    Pipelined     :: N -> Type -> IsPipelined ps

    -- | Non-pipelined peer.
    NonPipelined  :: IsPipelined ps

    -- | Pipelined peer for a /server/ that only ever uses one
    -- 'Network.TypedProtocol.Peer.Sender'
    AntiPipelined :: ps -> ps -> N -> IsPipelined ps

-- | Type level count of the number of outstanding pipelined yields for which
-- we have not yet collected a receiver result. Used to
-- ensure that 'Collect' is only used when there are outstanding results to
-- collect (e.g. after 'YieldPipeliend' was used);
-- and to ensure that the non-pipelined primitives 'Yield', 'Await' and 'Done'
-- are only used when there are none unsatisfied pipelined requests.
--
type        Outstanding :: IsPipelined ps -> N
type family Outstanding pl where
  Outstanding 'NonPipelined          = Z
  Outstanding ('Pipelined n _)       = n
  Outstanding ('AntiPipelined _ _ _) = Z

type        AntiOutstanding :: IsPipelined ps -> N
type family AntiOutstanding pl where
  AntiOutstanding 'NonPipelined          = Z
  AntiOutstanding ('Pipelined _ _)       = Z
  AntiOutstanding ('AntiPipelined _ _ n) = n

-- | A value level inductive natural number, indexed by the corresponding type
-- level natural number 'N'.
--
-- This is often needed when writing pipelined peers to be able to count the
-- number of outstanding pipelined yields, and show to the type checker that
-- 'Network.TypedProtocol.Peer.Collect' and 'Network.TypedProtocol.Peer.Done'
-- are being used correctly.
--
newtype Nat (n :: N) = UnsafeInt Int
  deriving Show via Int

data IsNat (n :: N) where
  IsZero ::          IsNat Z
  IsSucc :: Nat n -> IsNat (S n)

toIsNat :: Nat n -> IsNat n
toIsNat (UnsafeInt 0) = unsafeCoerce IsZero
toIsNat (UnsafeInt n) = unsafeCoerce (IsSucc (UnsafeInt (pred n)))

pattern Zero :: () => Z ~ n => Nat n
pattern Zero <- (toIsNat -> IsZero) where
  Zero = UnsafeInt 0

pattern Succ :: () => (m ~ S n) => Nat n -> Nat m
pattern Succ n <- (toIsNat -> IsSucc n) where
  Succ (UnsafeInt n) = UnsafeInt (succ n)

{-# COMPLETE Zero, Succ #-}

natToInt :: Nat n -> Int
natToInt (UnsafeInt n) = n

unsafeIntToNat :: Int -> Nat n
unsafeIntToNat = UnsafeInt

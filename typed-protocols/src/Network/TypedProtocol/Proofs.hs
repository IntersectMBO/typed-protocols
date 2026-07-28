{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}

-- This is already implied by the -Wall in the .cabal file, but lets just be
-- completely explicit about it too, since we rely on the completeness
-- checking in the cases below for the completeness of our proofs.
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Proofs and helpful testing utilities.
--
module Network.TypedProtocol.Proofs
  ( -- * Connect proofs
    connect
  , connectPipelined
  , connectLookahead
  , TerminalStates (..)
    -- * Pipelining proofs
    -- | Additional proofs specific to the pipelining features
  , forgetPipelined
  , promoteToPipelined
    -- * Lookahead proofs
    -- | Additional proofs specific to the lookahead features
  , forgetLookahead
  , promoteToLookahead
  , embedLookaheadUsingFixedSender
    -- ** Pipeline proof helpers
  , Queue (..)
  , enqueue
    -- ** Auxiliary functions
  , pipelineInterleaving
  ) where

import Data.Singletons
import Network.TypedProtocol.Core
import Network.TypedProtocol.Lemmas
import Network.TypedProtocol.Peer


-- | The 'connect' function takes two peers that agree on a protocol and runs
-- them in lock step, until (and if) they complete.
--
-- The 'connect' function serves a few purposes.
--
-- * The fact we can define this function at at all proves some minimal
-- sanity property of the typed protocol framework.
--
-- * It demonstrates that all protocols defined in the framework can be run
-- with synchronous communication rather than requiring buffered communication.
--
-- * It is useful for testing peer implementations against each other in a
-- minimalistic setting.
--
connect
   :: forall ps (pr :: PeerRole) (initSt :: ps) m a b.
      (Monad m, SingI pr)
   => Peer ps             pr  NonPipelined initSt m a
   -- ^ a peer
   -> Peer ps (FlipAgency pr) NonPipelined initSt m b
   -- ^ a peer with flipped agency
   -> m (a, b, TerminalStates ps)
   -- ^ peers results and an evidence of their termination
connect = go
  where
    singPeerRole :: Sing pr
    singPeerRole = sing

    go :: forall (st :: ps).
          Peer ps             pr  NonPipelined st m a
       -> Peer ps (FlipAgency pr) NonPipelined st m b
       -> m (a, b, TerminalStates ps)
    go (Done ReflNobodyAgency a)  (Done ReflNobodyAgency b) =
        return (a, b, terminals)
      where
        terminals :: TerminalStates ps
        terminals = TerminalStates (stateToken :: StateToken st)
                                   (stateToken :: StateToken st)

    go (Effect a )      b              = a >>= \a' -> go a' b
    go  a              (Effect b)      = b >>= \b' -> go a  b'
    go (Yield _ msg a) (Await _ b)     = go  a     (b msg)
    go (Await _ a)     (Yield _ msg b) = go (a msg) b

    -- By appealing to the proofs about agency for this protocol we can
    -- show that these other cases are impossible
    go (Yield reflA _ _) (Yield reflB _ _) =
      case exclusionLemma_ClientAndServerHaveAgency singPeerRole reflA reflB of
        ReflNobodyHasAgency -> case reflA of {}

    go (Await reflA _)   (Await reflB _)   =
      case exclusionLemma_ClientAndServerHaveAgency singPeerRole reflA reflB of
        ReflNobodyHasAgency -> case reflA of {}

    go (Done  reflA _) (Yield reflB _ _)   =
      case terminationLemma_2 singPeerRole reflB reflA of
        ReflNobodyHasAgency -> case reflB of {}

    go (Done  reflA _) (Await reflB _)     =
      case terminationLemma_2 singPeerRole reflB reflA of
        ReflNobodyHasAgency -> case reflB of {}

    go (Yield reflA _ _) (Done reflB _)    =
      case terminationLemma_1 singPeerRole reflA reflB of
        ReflNobodyHasAgency -> case reflA of {}

    go (Await reflA _)   (Done reflB _)    =
      case terminationLemma_1 singPeerRole reflA reflB of
        ReflNobodyHasAgency -> case reflA of {}


-- | The terminal states for the protocol. Used in 'connect' and
-- 'connectPipelined' to return the states in which the peers terminated.
--
data TerminalStates ps where
     TerminalStates
       :: forall ps (st :: ps).
          (StateAgency st  ~ NobodyAgency)
       => StateToken st
       -- ^ state termination evidence for the first peer
       -> StateToken st
       -- ^ state termination evidence for the second peer
       -> TerminalStates ps

--
-- Remove Pipelining
--


-- | A size indexed queue. This is useful for proofs, including
-- 'connectPipelined' but also as so-called @direct@ functions for running a
-- client and server wrapper directly against each other.
--
data Queue (n :: N) a where
  EmptyQ ::                   Queue  Z    a
  ConsQ  :: a -> Queue n a -> Queue (S n) a

-- | At an element to the end of a 'Queue'. This is not intended to be
-- efficient. It is only for proofs and tests.
--
enqueue :: a -> Queue n a -> Queue (S n) a
enqueue a  EmptyQ     = ConsQ a EmptyQ
enqueue a (ConsQ b q) = ConsQ b (enqueue a q)


-- | Proof that we have a total conversion from pipelined peers to regular
-- peers. This is a sanity property that shows that pipelining did not give
-- us extra expressiveness or to break the protocol state machine.
--
forgetPipelined
  :: forall ps (pr :: PeerRole) (st :: ps) m a.
     Functor m
  => [Bool]
  -- ^ interleaving choices for pipelining allowed by `Collect` primitive. False
  -- values or `[]` give no pipelining.
  -> PeerPipelined ps pr              st m a
  -> Peer          ps pr NonPipelined st m a
forgetPipelined cs0 (PeerPipelined peer) = goSender EmptyQ cs0 peer
  where
    goSender :: forall st' n c.
                Queue n c
             -> [Bool]
             -> Peer ps pr ('Pipelined n c) st' m a
             -> Peer ps pr 'NonPipelined    st' m a

    goSender EmptyQ _cs (Done           refl     k) = Done refl k
    goSender q       cs (Effect                  k) = Effect (goSender q cs <$> k)
    goSender q       cs (Yield          refl m   k) = Yield refl m (goSender q cs k)
    goSender q       cs (Await          refl     k) = Await refl   (goSender q cs <$> k)
    goSender q       cs (YieldPipelined refl m r k) = Yield refl m (goReceiver q cs k r)
    goSender q (True:cs')       (Collect (Just k) _) = goSender q cs' k
    goSender (ConsQ x q) (_:cs) (Collect _ k)        = goSender q cs (k x)
    goSender (ConsQ x q) cs@[]  (Collect _ k)        = goSender q cs (k x)

    goReceiver :: forall stCurrent stNext n c.
                  Queue n c
               -> [Bool]
               -> Peer     ps pr ('Pipelined (S n) c) stNext m a
               -> Receiver ps pr  stCurrent stNext m c
               -> Peer     ps pr 'NonPipelined stCurrent m a

    goReceiver q cs s (ReceiverDone     x) = goSender (enqueue x q) cs s
    goReceiver q cs s (ReceiverEffect   k) = Effect   (goReceiver q cs s <$> k)
    goReceiver q cs s (ReceiverAwait refl k) = Await refl (goReceiver q cs s . k)


-- | Promote a peer to a pipelined one.
--
-- This is a right inverse of `forgetPipelined`, e.g.
--
-- >>> forgetPipelined . promoteToPipelined = id
--
promoteToPipelined
  :: forall ps (pr :: PeerRole) st m a.
     Functor m
  => Peer          ps pr NonPipelined st m a
  -- ^ a peer
  -> PeerPipelined ps pr              st m a
  -- ^ a pipelined peer
promoteToPipelined p = PeerPipelined (go p)
  where
    go :: forall st' c.
          Peer ps pr NonPipelined    st' m a
       -> Peer ps pr (Pipelined Z c) st' m a
    go (Effect k)         = Effect $ go <$> k
    go (Yield refl msg k) = Yield refl msg (go k)
    go (Await refl k)     = Await refl (go . k)
    go (Done refl k)      = Done refl k


-- | Analogous to 'connect' but also for pipelined peers.
--
-- Since pipelining allows multiple possible interleavings, we provide a
-- @[Bool]@ parameter to control the choices. Each @True@ will trigger picking
-- the first choice in the @SenderCollect@ construct (if possible), leading
-- to more results outstanding. This can also be interpreted as a greater
-- pipeline depth, or more messages in-flight.
--
-- This can be exercised using a QuickCheck style generator.
--
connectPipelined
  :: forall ps (pr :: PeerRole)
               (st :: ps) m a b.
       (Monad m, SingI pr)
    => [Bool]
    -- ^ an interleaving
    -> PeerPipelined ps             pr               st m a
    -- ^ a pipelined peer
    -> Peer          ps (FlipAgency pr) NonPipelined st m b
    -- ^ a non-pipelined peer with fliped agency
    -> m (a, b, TerminalStates ps)
    -- ^ peers results and an evidence of their termination
connectPipelined csA a b =
    connect (forgetPipelined csA a) b


--
-- Remove Lookahead
--


-- | Total conversion from lookahead peers to regular peers: the lookahead
-- analogue of 'forgetPipelined'.
--
-- Where 'forgetPipelined' inlines each 'Receiver' at the point it is collected,
-- this inlines the 'Sender' supplied at each 'AwaitLookahead': its sends, which
-- the sender thread would have performed asynchronously, are performed
-- synchronously just before the fused receive.
--
-- Dually to 'forgetPipelined', the @[Bool]@ chooses the interleaving at each
-- 'FlushSender': a @True@ pretends the 'Sender' has not finished yet, so the
-- peer takes its non-blocking continuation (when it has one) and leaves the
-- send outstanding; a @False@ (or @[]@) flushes it.
--
forgetLookahead
  :: forall ps (pr :: PeerRole) (st :: ps) m a.
     Functor m
  => [Bool]
  -- ^ interleaving choices allowed by the 'FlushSender' primitive; @False@
  -- values or @[]@ leave nothing outstanding.
  -> PeerLookahead ps pr              st m a
  -> Peer          ps pr NonPipelined st m a
forgetLookahead cs0 (PeerLookahead peer0) =
    goPeer cs0 peer0
  where
    goPeer :: forall st' n.
              [Bool]
           -> Peer ps pr ('Lookahead n VariableSender) st' m a
           -> Peer ps pr 'NonPipelined                 st' m a
    goPeer cs (Effect               k) = Effect (goPeer cs <$> k)
    goPeer _  (Done  refl           k) = Done refl k
    goPeer cs (Yield refl m         k) = Yield refl m (goPeer cs k)
    goPeer cs (Await refl           k) = Await refl (goPeer cs . k)
    goPeer cs (AwaitLookahead refl sender k) =
        goSender sender (Await refl (goPeer cs . k))
    goPeer (True:cs') (FlushSender (Just k) _) = goPeer cs' k
    goPeer (_:cs)     (FlushSender _ k)        = goPeer cs  k
    goPeer cs@[]      (FlushSender _ k)        = goPeer cs  k

    goSender :: forall sst sst'.
                Sender ps pr VariableSender sst sst' m
             -> Peer   ps pr 'NonPipelined sst' m a
             -> Peer   ps pr 'NonPipelined sst  m a
    goSender  SenderDone             k = k
    goSender (SenderEffect       ks) k = Effect ((`goSender` k) <$> ks)
    goSender (SenderYield refl m ks) k = Yield refl m (goSender ks k)


-- | Promote a peer to a lookahead one, using an empty 'Sender'.
--
-- This is a right inverse of 'forgetLookahead', e.g.
--
-- >>> forgetLookahead . promoteToLookahead = id
--
promoteToLookahead
  :: forall ps (pr :: PeerRole) (st :: ps) m a.
     Functor m
  => Peer          ps pr NonPipelined st m a
  -- ^ a peer
  -> PeerLookahead ps pr              st m a
  -- ^ a lookahead peer
promoteToLookahead p = PeerLookahead (go p)
  where
    go :: forall st'.
          Peer ps pr 'NonPipelined                  st' m a
       -> Peer ps pr ('Lookahead 'Z VariableSender) st' m a
    go (Effect         k) = Effect (go <$> k)
    go (Yield refl m   k) = Yield refl m (go k)
    go (Await refl     k) = Await refl (go . k)
    go (Done  refl     k) = Done refl k


-- | Embed a fixed-'Sender' lookahead peer as a variable-'Sender' one, by
-- plugging the carried 'Sender' in for each 'TheSender'. This lets the
-- 'PeerLookahead' machinery (proofs, driver) subsume 'PeerLookaheadFixedSender' —
-- though a dedicated fixed driver is still preferable, as it can track
-- outstanding sends with a counter rather than a queue.
--
embedLookaheadUsingFixedSender
  :: forall ps (pr :: PeerRole) (st :: ps) m a.
     Functor m
  => PeerLookaheadFixedSender ps pr st m a
  -> PeerLookahead            ps pr st m a
embedLookaheadUsingFixedSender (PeerLookaheadFixedSender (sender :: Sender ps pr VariableSender apst apst' m) peer0) =
    PeerLookahead (go peer0)
  where
    go :: forall st' n.
          Peer ps pr ('Lookahead n (FixedSender apst apst')) st' m a
       -> Peer ps pr ('Lookahead n VariableSender)           st' m a
    go (Effect               k) = Effect (go <$> k)
    go (Done  refl           k) = Done refl k
    go (Yield refl m         k) = Yield refl m (go k)
    go (Await refl           k) = Await refl (go . k)
    go (AwaitLookahead refl TheSender k) =
        AwaitLookahead refl sender (go . k)
    go (FlushSender mk k)       = FlushSender (go <$> mk) (go k)


-- | Analogous to 'connectPipelined' but for lookahead peers.
--
connectLookahead
  :: forall ps (pr :: PeerRole)
               (st :: ps) m a b.
       (Monad m, SingI pr)
    => [Bool]
    -- ^ an interleaving
    -> PeerLookahead ps             pr               st m a
    -- ^ a lookahead peer
    -> Peer          ps (FlipAgency pr) NonPipelined st m b
    -- ^ a non-pipelined peer with flipped agency
    -> m (a, b, TerminalStates ps)
    -- ^ peers results and an evidence of their termination
connectLookahead cs a b =
    connect (forgetLookahead cs a) b


-- | A reference specification for interleaving of requests and responses
-- with pipelining, where the environment can choose whether a response is
-- available yet.
--
-- This also supports bounded choice where the maximum number of outstanding
-- in-flight responses is limited.
--
pipelineInterleaving :: Int    -- ^ Bound on outstanding responses
                     -> [Bool] -- ^ Pipelining choices
                     -> [req] -> [resp] -> [Either req resp]
pipelineInterleaving omax cs0 reqs0 resps0 =
    go 0 cs0 (zip [0 :: Int ..] reqs0)
             (zip [0 :: Int ..] resps0)
  where
    go o (c:cs) reqs@((reqNo, req) :reqs')
               resps@((respNo,resp):resps')
      | respNo == reqNo = Left  req   : go (o+1) (c:cs) reqs' resps
      | c && o < omax   = Left  req   : go (o+1)    cs  reqs' resps
      | otherwise       = Right resp  : go (o-1)    cs  reqs  resps'

    go o []     reqs@((reqNo, req) :reqs')
               resps@((respNo,resp):resps')
      | respNo == reqNo = Left  req   : go (o+1) [] reqs' resps
      | otherwise       = Right resp  : go (o-1) [] reqs  resps'

    go _ _ [] resps     = map (Right . snd) resps
    go _ _ (_:_) []     = error "pipelineInterleaving: not enough responses"

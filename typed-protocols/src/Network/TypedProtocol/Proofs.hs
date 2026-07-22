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
  , connectAntiPipelined
  , TerminalStates (..)
    -- * Pipelining proofs
    -- | Additional proofs specific to the pipelining features
  , forgetPipelined
  , promoteToPipelined
    -- * Anti-pipelining proofs
    -- | Additional proofs specific to the anti-pipelining features
  , forgetAntiPipelined
  , promoteToAntiPipelined
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
    goSender q       cs (Yield         refl m   k) = Yield refl m (goSender q cs k)
    goSender q       cs (Await          refl     k) = Await refl   (goSender q cs <$> k)
    goSender q       cs (AwaitPipelined      r   k) = goReceiver q cs k r
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
-- The @[Bool]@ stream permits exploration of some interleavings, but not
-- all. In particular, it does not explore different possible interleavings of
-- the 'ReceiverEffect' with the 'Pipelined' peer's 'Effect's: the full
-- 'Receiver' is always executed immediately first.
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
-- Remove Anti-Pipelining
--


-- | Proof that we have a total conversion from anti-pipelined peers to regular
-- peers. This is the anti-pipelining analogue of 'forgetPipelined'.
--
-- Where 'forgetPipelined' inlines each deferred 'Receiver' at the point it is
-- collected, this inlines the single 'Sender' at each 'YieldAntiPipelined':
-- the messages the sender thread would have sent asynchronously are instead
-- sent synchronously, at the point the peer delegated them.
--
-- Dually to 'forgetPipelined', the @[Bool]@ chooses the interleaving at each
-- 'AntiCollect': a @True@ pretends the delegated 'Sender' has not terminated
-- yet, so the peer takes its non-blocking continuation (when it has one) and
-- leaves the send outstanding; a @False@ (or @[]@) collects.
--
forgetAntiPipelined
  :: forall ps (pr :: PeerRole) (st :: ps) m a.
     Functor m
  => [Bool]
  -- ^ interleaving choices for anti-pipelining allowed by `AntiCollect` primitive.
  -- False values or `[]` give no anti-pipelining.
  -> PeerAntiPipelined ps pr              st m a
  -> Peer              ps pr NonPipelined st m a
forgetAntiPipelined cs0 (PeerAntiPipelined (sender :: Sender ps pr apst apst' m) peer0) =
    goPeer cs0 peer0
  where
    goPeer :: forall st' n.
              [Bool]
           -> Peer ps pr ('AntiPipelined apst apst' n) st' m a
           -> Peer ps pr 'NonPipelined                 st' m a
    goPeer cs (Effect               k) = Effect (goPeer cs <$> k)
    goPeer _  (Done  refl           k) = Done refl k
    goPeer cs (Yield refl m         k) = Yield refl m (goPeer cs k)
    goPeer cs (Await refl           k) = Await refl (goPeer cs . k)
    goPeer cs (YieldAntiPipelined   k) = goSender sender (goPeer cs k)
    goPeer (True:cs') (AntiCollect _ (Just k)) = goPeer cs' k
    goPeer (_:cs)     (AntiCollect k _)        = goPeer cs  k
    goPeer cs@[]      (AntiCollect k _)        = goPeer cs  k

    goSender :: forall sst.
                Sender ps pr sst apst' m
             -> Peer   ps pr 'NonPipelined apst' m a
             -> Peer   ps pr 'NonPipelined sst   m a
    goSender  SenderDone             k = k
    goSender (SenderEffect       ks) k = Effect ((`goSender` k) <$> ks)
    goSender (SenderYield refl m ks) k = Yield refl m (goSender ks k)


-- | Promote a peer to an anti-pipelined one, using an empty 'Sender'.
--
-- This is a right inverse of 'forgetAntiPipelined', e.g.
--
-- >>> forgetAntiPipelined . promoteToAntiPipelined = id
--
promoteToAntiPipelined
  :: forall ps (pr :: PeerRole) (st :: ps) m a.
     Functor m
  => Peer              ps pr NonPipelined st m a
  -- ^ a peer
  -> PeerAntiPipelined ps pr              st m a
  -- ^ an anti-pipelined peer
promoteToAntiPipelined p = PeerAntiPipelined SenderDone (go p)
  where
    go :: forall st'.
          Peer ps pr 'NonPipelined            st' m a
       -> Peer ps pr ('AntiPipelined st st 'Z) st' m a
    go (Effect         k) = Effect (go <$> k)
    go (Yield refl m   k) = Yield refl m (go k)
    go (Await refl     k) = Await refl (go . k)
    go (Done  refl     k) = Done refl k


-- | Analogous to 'connectPipelined' but for anti-pipelined peers.
--
connectAntiPipelined
  :: forall ps (pr :: PeerRole)
               (st :: ps) m a b.
       (Monad m, SingI pr)
    => [Bool]
    -- ^ an interleaving
    -> PeerAntiPipelined ps             pr               st m a
    -- ^ an anti-pipelined peer
    -> Peer              ps (FlipAgency pr) NonPipelined st m b
    -- ^ a non-pipelined peer with flipped agency
    -> m (a, b, TerminalStates ps)
    -- ^ peers results and an evidence of their termination
connectAntiPipelined cs a b =
    connect (forgetAntiPipelined cs a) b

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

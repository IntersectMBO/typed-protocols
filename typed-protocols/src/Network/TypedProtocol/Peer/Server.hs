{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators   #-}

-- | Bidirectional patterns for @'Peer' ps 'AsServer'@.   The advantage of
-- these patterns is that they automatically provide the 'ReflRelativeAgency'
-- evidence.
--
module Network.TypedProtocol.Peer.Server
  ( -- * Server type alias and its pattern synonyms
    Server
  , pattern Effect
  , pattern Yield
  , pattern Await
  , pattern Done
  , pattern YieldPipelined
  , pattern Collect
  , pattern YieldDualPipelined1
  , pattern DualCollect1
    -- * Receiver type alias and its pattern synonyms
  , Receiver
  , pattern ReceiverEffect
  , pattern ReceiverAwait
  , pattern ReceiverDone
    -- * Sender type alias and its pattern synonyms
  , Sender
  , pattern SenderEffect
  , pattern SenderYield
  , pattern SenderDone
    -- * ServerPipelined type alias and its pattern synonym
  , ServerPipelined
  , TP.PeerPipelined (ServerPipelined, runServerPipelined)
    -- * ServerDualPipelined1 type alias and its pattern synonym
  , ServerDualPipelined1
  , pattern ServerDualPipelined1
    -- * re-exports
  , IsPipelined (..)
  , Outstanding
  , DualOutstanding
  , N (..)
  , Nat (..)
  ) where

import Data.Kind (Type)

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer (Peer)
import Network.TypedProtocol.Peer qualified as TP


type Server :: forall ps
            -> IsPipelined ps
            -> ps
            -> (Type -> Type)
            -> Type
            -> Type
type Server ps pl st m a = Peer ps AsServer pl st m a


-- | A description of a peer that engages in a protocol in a pipelined fashion.
--
type ServerPipelined  ps st m a = TP.PeerPipelined ps AsServer st m a

pattern ServerPipelined :: forall ps st m a.
                           ()
                        => forall c.
                           ()
                        => Server ps (Pipelined Z c) st m a
                        -> ServerPipelined ps st m a
pattern ServerPipelined { runServerPipelined } = TP.PeerPipelined runServerPipelined

{-# COMPLETE ServerPipelined #-}


-- | A description of a peer that engages in a protocol in an anti-pipelined
-- fashion (ie non-blocking sends).
--
type ServerDualPipelined1 ps st m a = TP.PeerDualPipelined1 ps AsServer st m a

pattern ServerDualPipelined1 :: forall ps st m a.
                               ()
                            => forall apst apst'.
                               ()
                            => Sender ps apst apst' m
                            -> Server ps (DualPipelined1 apst apst' Z) st m a
                            -> ServerDualPipelined1 ps st m a
pattern ServerDualPipelined1 sender peer = TP.PeerDualPipelined1 sender peer

{-# COMPLETE ServerDualPipelined1 #-}


-- | Server role pattern for 'TP.Effect'.
--
pattern Effect :: forall ps pl st m a.
                  m (Server ps pl st m a)
               -- ^ monadic continuation
               -> Server ps pl st m a
pattern Effect mclient = TP.Effect mclient


-- | Server role pattern for 'TP.Yield'
--
pattern Yield :: forall ps pl st m a.
                 ()
              => forall st'.
                 ( StateTokenI st
                 , StateTokenI st'
                 , StateAgency st ~ ServerAgency
                 , Outstanding pl ~ Z
                 , DualOutstanding pl ~ Z
                 )
              => Message ps st st'
              -- ^ protocol message
              -> Server ps pl st' m a
              -- ^ continuation
              -> Server ps pl st  m a
pattern Yield msg k = TP.Yield ReflServerAgency msg k


-- | Server role pattern for 'TP.Await'
--
pattern Await :: forall ps pl st m a.
                 ()
              => ( StateTokenI st
                 , StateAgency st ~ ClientAgency
                 , Outstanding pl ~ Z
                 )
              => (forall st'. Message ps st st'
                  -> Server ps pl st' m a)
              -- ^ continuation
              -> Server     ps pl st  m a
pattern Await k = TP.Await ReflClientAgency k


-- | Server role pattern for 'TP.Done'
--
pattern Done :: forall ps pl st m a.
                ()
             => ( StateTokenI st
                , StateAgency st ~ NobodyAgency
                , Outstanding pl ~ Z
                , DualOutstanding pl ~ Z
                )
             => a
             -- ^ protocol return value
             -> Server ps pl st m a
pattern Done a = TP.Done ReflNobodyAgency a


-- | Server role pattern for 'TP.YieldPipelined'
--
pattern YieldPipelined :: forall ps st n c m a.
                          ()
                       => forall st' st''.
                          ( StateTokenI st
                          , StateTokenI st'
                          , StateAgency st ~ ServerAgency
                          )
                       => Message ps st st'
                       -- ^ pipelined message
                       -> Receiver ps st' st'' m c
                       -> Server ps (Pipelined (S n) c) st'' m a
                       -- ^ continuation
                       -> Server ps (Pipelined    n  c)  st   m a
pattern YieldPipelined msg receiver k = TP.YieldPipelined ReflServerAgency msg receiver k


-- | Server role pattern for 'TP.Collect'
--
pattern Collect :: forall ps st n c m a.
                   ()
                => ( StateTokenI st
                   , ActiveState st
                   )
                => Maybe (Server ps (Pipelined (S n) c) st m a)
                -- ^ continuation, executed if no message has arrived so far
                -> (c -> Server  ps (Pipelined    n  c)  st m a)
                -- ^ continuation
                -> Server        ps (Pipelined (S n) c) st m a
pattern Collect k' k = TP.Collect k' k


{-# COMPLETE Effect, Yield, Await, Done, YieldPipelined, Collect  #-}


-- | Server role pattern for 'TP.YieldDualPipelined1'
--
pattern YieldDualPipelined1 :: forall ps st st' n m a.
                              ()
                           => ( StateTokenI st
                              , StateTokenI st'
                              )
                           => Server ps (DualPipelined1 st st' (S n)) st' m a
                           -- ^ continuation, before or after sending
                           -> Server ps (DualPipelined1 st st'  n ) st  m a
pattern YieldDualPipelined1 k = TP.YieldDualPipelined1 k


-- | Server role pattern for 'TP.DualCollect1'
--
pattern DualCollect1 :: forall ps st apst apst' n m a.
                       ()
                    => StateTokenI st
                    => Server ps (DualPipelined1 apst apst'   n ) st m a
                    -- ^ continuation if the 'Sender' has already terminated
                    -> Maybe (Server ps (DualPipelined1 apst apst' (S n)) st m a)
                    -- ^ continuation if the 'Sender' may not have terminated
                    -> Server ps (DualPipelined1 apst apst' (S n)) st m a
pattern DualCollect1 k mk = TP.DualCollect1 k mk

{-# COMPLETE Effect, Yield, Await, Done, YieldDualPipelined1, DualCollect1 #-}


type Receiver ps st stdone m c = TP.Receiver ps AsServer st stdone m c

pattern ReceiverEffect :: forall ps st stdone m c.
                          m (Receiver ps st stdone m c)
                       -> Receiver ps st stdone m c
pattern ReceiverEffect k = TP.ReceiverEffect k

pattern ReceiverAwait :: forall ps st stdone m c.
                         ()
                      => ( StateTokenI st
                         , ActiveState st
                         , StateAgency st ~ ClientAgency
                         )
                      => (forall st'. Message  ps st st'
                                   -> Receiver ps    st' stdone m c
                         )
                      -> Receiver ps st stdone m c
pattern ReceiverAwait k = TP.ReceiverAwait ReflClientAgency k

pattern ReceiverDone :: forall ps stdone m c.
                        c
                     -> Receiver ps stdone stdone m c
pattern ReceiverDone c = TP.ReceiverDone c

{-# COMPLETE ReceiverEffect, ReceiverAwait, ReceiverDone #-}


type Sender ps st stdone m = TP.Sender ps AsServer st stdone m

pattern SenderEffect :: forall ps st stdone m.
                        m (Sender ps st stdone m)
                     -> Sender ps st stdone m
pattern SenderEffect k = TP.SenderEffect k

pattern SenderYield :: forall ps st stdone m.
                       ()
                    => forall st'.
                       ( StateTokenI st
                       , StateTokenI st'
                       , StateAgency st ~ ServerAgency
                       )
                    => Message ps st st'
                    -> Sender ps st' stdone m
                    -> Sender ps st  stdone m
pattern SenderYield msg k = TP.SenderYield ReflServerAgency msg k

pattern SenderDone :: forall ps stdone m.
                      Sender ps stdone stdone m
pattern SenderDone = TP.SenderDone

{-# COMPLETE SenderEffect, SenderYield, SenderDone #-}

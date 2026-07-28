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
  , pattern AwaitLookahead
  , pattern FlushSender
    -- * Receiver type alias and its pattern synonyms
  , Receiver
  , pattern ReceiverEffect
  , pattern ReceiverAwait
  , pattern ReceiverDone
    -- * Sender type alias and its pattern synonyms
  , Sender
  , pattern TheSender
  , pattern SenderEffect
  , pattern SenderYield
  , pattern SenderDone
    -- * ServerPipelined type alias and its pattern synonym
  , ServerPipelined
  , TP.PeerPipelined (ServerPipelined, runServerPipelined)
    -- * ServerLookahead type aliases and their pattern synonyms
  , ServerLookahead
  , TP.PeerLookahead (ServerLookahead, runServerLookahead)
  , ServerLookaheadFixedSender
  , pattern ServerLookaheadFixedSender
    -- * re-exports
  , IsPipelined (..)
  , SenderVariability (..)
  , Outstanding
  , OutstandingSenders
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


-- TODO: mirror these lookahead pattern synonyms in
-- 'Network.TypedProtocol.Peer.Client' (a 'ClientLookahead' /
-- 'ClientLookaheadFixedSender').  They are not strictly needed since the API is
-- symmetric, but they would let a user work in terms of 'Client' as well as
-- 'Server'.

-- | A lookahead server that supplies its own 'Sender' at each 'AwaitLookahead'.
--
type ServerLookahead ps st m a = TP.PeerLookahead ps AsServer st m a

pattern ServerLookahead :: forall ps st m a.
                           ()
                        => Server ps (Lookahead Z VariableSender) st m a
                        -> ServerLookahead ps st m a
pattern ServerLookahead { runServerLookahead } = TP.PeerLookahead runServerLookahead

{-# COMPLETE ServerLookahead #-}


-- | A lookahead server that reuses one 'Sender' at each 'AwaitLookaheadFixedSender'.
--
type ServerLookaheadFixedSender ps st m a = TP.PeerLookaheadFixedSender ps AsServer st m a

pattern ServerLookaheadFixedSender :: forall ps st m a.
                                ()
                             => forall apst apst'.
                                ()
                             => Sender ps VariableSender apst apst' m
                             -> Server ps (Lookahead Z (FixedSender apst apst')) st m a
                             -> ServerLookaheadFixedSender ps st m a
pattern ServerLookaheadFixedSender sender peer = TP.PeerLookaheadFixedSender sender peer

{-# COMPLETE ServerLookaheadFixedSender #-}


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
                 , OutstandingSenders pl ~ Z
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
                 , OutstandingSenders pl ~ Z
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
                , OutstandingSenders pl ~ Z
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


-- | Server role pattern for 'TP.AwaitLookahead'
--
-- Use 'TheSender' as the first argument for a fixed-'Sender' peer, or a
-- concrete 'VariableSender' for a per-step one.
--
pattern AwaitLookahead :: forall ps sv st n m a.
                          ()
                       => forall st'.
                          ( StateTokenI st
                          , StateTokenI st'
                          , ActiveState st
                          , StateAgency st' ~ ClientAgency
                          )
                       => Sender ps sv st st' m
                       -- ^ sender for the deferred send @st -> st'@
                       -> (forall st''. Message ps st' st''
                           -> Server ps (Lookahead (S n) sv) st'' m a)
                       -- ^ continuation, awaiting ahead at @st'@
                       -> Server ps (Lookahead n sv) st m a
pattern AwaitLookahead sender k = TP.AwaitLookahead ReflClientAgency sender k


-- | Server role pattern for 'TP.FlushSender'
--
pattern FlushSender :: forall ps st n sv m a.
                       ()
                    => ( StateTokenI st
                       )
                    => Maybe (Server ps (Lookahead (S n) sv) st m a)
                    -- ^ continuation if no 'Sender' has finished so far
                    -> (Server ps (Lookahead n sv) st m a)
                    -- ^ continuation once a 'Sender' has finished
                    -> Server ps (Lookahead (S n) sv) st m a
pattern FlushSender mk k = TP.FlushSender mk k


{-# COMPLETE Effect, Yield, Await, Done, AwaitLookahead, FlushSender #-}


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


type Sender ps sv st stdone m = TP.Sender ps AsServer sv st stdone m

pattern TheSender :: forall ps sv st stdone m.
                     ()
                  => (sv ~ FixedSender st stdone)
                  => Sender ps sv st stdone m
pattern TheSender = TP.TheSender

pattern SenderEffect :: forall ps st stdone m.
                        m (Sender ps VariableSender st stdone m)
                     -> Sender ps VariableSender st stdone m
pattern SenderEffect k = TP.SenderEffect k

pattern SenderYield :: forall ps st stdone m.
                       ()
                    => forall st'.
                       ( StateTokenI st
                       , StateTokenI st'
                       , StateAgency st ~ ServerAgency
                       )
                    => Message ps st st'
                    -> Sender ps VariableSender st' stdone m
                    -> Sender ps VariableSender st  stdone m
pattern SenderYield msg k = TP.SenderYield ReflServerAgency msg k

pattern SenderDone :: forall ps stdone m.
                      Sender ps VariableSender stdone stdone m
pattern SenderDone = TP.SenderDone

{-# COMPLETE TheSender #-}
{-# COMPLETE SenderEffect, SenderYield, SenderDone #-}

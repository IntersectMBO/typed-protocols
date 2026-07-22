{-# LANGUAGE PatternSynonyms #-}

-- | Bidirectional patterns for @'Peer' ps 'AsClient'@.   The advantage of
-- these patterns is that they automatically provide the 'ReflRelativeAgency'
-- evidence.
--
module Network.TypedProtocol.Peer.Client
  ( -- * Client type alias and its pattern synonyms
    Client
  , pattern Effect
  , pattern Yield
  , pattern Await
  , pattern Done
  , pattern YieldPipelined
  , pattern Collect
    -- * Receiver type alias and its pattern synonyms
  , Receiver
  , pattern ReceiverEffect
  , pattern ReceiverAwait
  , pattern ReceiverDone
    -- * ClientPipelined type alias and its pattern synonym
  , ClientPipelined
  , TP.PeerPipelined (ClientPipelined, runClientPipelined)
    -- * re-exports
  , IsPipelined (..)
  , Outstanding
  , N (..)
  , Nat (..)
  ) where

import Data.Kind (Type)

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer (Peer)
import Network.TypedProtocol.Peer qualified as TP


type Client :: forall ps
            -> IsPipelined ps
            -> ps
            -> (Type -> Type)
            -> Type
            -> Type
type Client ps pl st m a = Peer ps AsClient pl st m a


-- | A description of a peer that engages in a protocol in a pipelined fashion.
--
type ClientPipelined  ps st m a = TP.PeerPipelined ps AsClient st m a

pattern ClientPipelined :: forall ps st m a.
                           ()
                        => forall c.
                           ()
                        => Client ps (Pipelined Z c) st m a
                        -> ClientPipelined ps st m a
pattern ClientPipelined { runClientPipelined } = TP.PeerPipelined runClientPipelined

{-# COMPLETE ClientPipelined #-}

-- | Client role pattern for 'TP.Effect'.
--
pattern Effect :: forall ps pl st m a.
                  m (Client ps pl st m a)
               -- ^ monadic continuation
               -> Client ps pl st m a
pattern Effect mclient = TP.Effect mclient


-- | Client role pattern for 'TP.Yield'
--
pattern Yield :: forall ps pl st m a.
                 ()
              => forall st'.
                 ( StateTokenI st
                 , StateTokenI st'
                 , StateAgency st ~ ClientAgency
                 , AntiOutstanding pl ~ Z
                 )
              => Message ps st st'
              -- ^ protocol message
              -> Client ps pl st' m a
              -- ^ continuation
              -> Client ps pl st  m a
pattern Yield msg k = TP.Yield2 ReflClientAgency msg k


-- | Client role pattern for 'TP.Await'
--
pattern Await :: forall ps pl st m a.
                 ()
              => ( StateTokenI st
                 , StateAgency st ~ ServerAgency
                 , Outstanding pl ~ Z
                 )
              => (forall st'. Message ps st st'
                  -> Client ps pl st' m a)
              -- ^ continuation
              -> Client     ps pl st  m a
pattern Await k = TP.Await ReflServerAgency k


-- | Client role pattern for 'TP.Done'
--
pattern Done :: forall ps pl st m a.
                ()
             => ( StateTokenI st
                , StateAgency st ~ NobodyAgency
                , Outstanding pl ~ Z
                , AntiOutstanding pl ~ Z
                )
             => a
             -- ^ protocol return value
             -> Client ps pl st m a
pattern Done a = TP.Done ReflNobodyAgency a


-- | Client role pattern for 'TP.YieldPipelined'
--
pattern YieldPipelined :: forall ps st n c m a.
                          ()
                       => forall st' st''.
                          ( StateTokenI st
                          , StateTokenI st'
                          , StateAgency st ~ ClientAgency
                          )
                       => Message ps st st'
                       -- ^ pipelined message
                       -> Receiver ps st' st'' m c
                       -> Client ps (Pipelined (S n) c) st'' m a
                       -- ^ continuation
                       -> Client ps (Pipelined    n  c)  st   m a
pattern YieldPipelined msg receiver k = TP.Yield2 ReflClientAgency msg (TP.AwaitPipelined receiver k)


-- | Client role pattern for 'TP.Collect'
--
pattern Collect :: forall ps st n c m a.
                   ()
                => ( StateTokenI st
                   , ActiveState st
                   )
                => Maybe (Client ps (Pipelined (S n) c) st m a)
                -- ^ continuation, executed if no message has arrived so far
                -> (c ->  Client ps (Pipelined    n  c)  st m a)
                -- ^ continuation
                -> Client        ps (Pipelined (S n) c) st m a
pattern Collect k' k = TP.Collect k' k

{-# COMPLETE Effect, Yield, Await, Done, YieldPipelined, Collect #-}


type Receiver ps st stdone m c = TP.Receiver ps AsClient st stdone m c

pattern ReceiverEffect :: forall ps st stdone m c.
                          m (Receiver ps st stdone m c)
                       -> Receiver ps st stdone m c
pattern ReceiverEffect k = TP.ReceiverEffect k

pattern ReceiverAwait :: forall ps st stdone m c.
                         ()
                      => ( StateTokenI st
                         , ActiveState st
                         , StateAgency st ~ ServerAgency
                         )
                      => (forall st'. Message  ps st st'
                                   -> Receiver ps    st' stdone m c
                         )
                      -> Receiver ps st stdone m c
pattern ReceiverAwait k = TP.ReceiverAwait ReflServerAgency k

pattern ReceiverDone :: forall ps stdone m c.
                        c
                     -> Receiver ps stdone stdone m c
pattern ReceiverDone c = TP.ReceiverDone c

{-# COMPLETE ReceiverEffect, ReceiverAwait, ReceiverDone #-}

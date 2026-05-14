{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
-- @UndecidableInstances@ extension is required for defining @Show@ instance of
-- @'AnyMessage'@ and @'AnyMessage'@.
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}

-- | Stateful codec.  This module is intended to be imported qualified.
--
module Network.TypedProtocol.Stateful.Codec
  ( -- * Defining and using Codecs
    Codec (..)
  , hoistCodec
  , isoCodec
  , mapFailureCodec
  , liftCodec
    -- ** Incremental decoding
  , DecodeStep (..)
  , runDecoder
  , runDecoderPure
    -- ** Related types
    -- *** SomeMessage
  , SomeMessage (..)
    -- *** StateToken
  , StateToken
  , StateTokenI (..)
    -- *** ActiveState
  , ActiveState
    -- *** PeerRole
  , PeerRole (..)
    -- * CodecFailure
  , CodecFailure (..)
    -- * Testing codec properties
  , AnyMessage (.., AnyMessageAndAgency)
  , showAnyMessage
  ) where

import Data.Kind (Type)

import Network.TypedProtocol.Codec (CodecFailure (..), DecodeStep (..),
           SomeMessage (..), hoistDecodeStep, isoDecodeStep,
           mapFailureDecodeStep, runDecoder, runDecoderPure)
import Network.TypedProtocol.Codec qualified as TP hiding (AnyMessageAndAgency)
import Network.TypedProtocol.Core


-- | A stateful codec.
--
-- TODO: provide CodecF as in typed-protocols:typed-protocols library.
--
data Codec ps failure (f :: ps -> Type) m bytes = Codec {
       encode :: forall (st :: ps) (st' :: ps).
                 StateTokenI st
              => ActiveState st
              => f st
              -- local state, which contain extra context for the encoding
              -- process.
              --
              -- TODO: intersectmbo/typed-protocols#57
              -> Message ps st st'
              -- message to be encoded
              -> bytes,

       decode :: forall (st :: ps).
                 ActiveState st
              => StateToken st
              -> f st
              -- local state, which can contain extra context from the
              -- previous message.
              --
              -- TODO: intersectmbo/typed-protocols#57
              -> m (DecodeStep bytes failure m (SomeMessage st))
     }

liftCodec :: TP.Codec ps failure m bytes -> Codec ps failure f m bytes
liftCodec codec = Codec { encode = \_ msg -> TP.encode codec msg
                        , decode = \stok _ -> TP.decode codec stok
                        }

hoistCodec
  :: ( Functor n )
  => (forall x . m x -> n x)
  -> Codec ps failure f m bytes
  -> Codec ps failure f n bytes
hoistCodec nat codec = codec
  { decode = \stok f -> fmap (hoistDecodeStep nat) . nat $ decode codec stok f
  }

isoCodec :: Functor m
         => (bytes -> bytes')
         -> (bytes' -> bytes)
         -> Codec ps failure f m bytes
         -> Codec ps failure f m bytes'
isoCodec g finv Codec {encode, decode} = Codec {
      encode = \f msg -> g $ encode f msg,
      decode = \stok f -> isoDecodeStep g finv <$> decode stok f
    }

mapFailureCodec
  :: Functor m
  => (failure -> failure')
  -> Codec ps failure  f m bytes
  -> Codec ps failure' f m bytes
mapFailureCodec g Codec {encode, decode} = Codec {
    encode = encode,
    decode = \stok f -> mapFailureDecodeStep g <$> decode stok f
  }


--
-- Codec properties
--

-- | Any message for a protocol, with a 'StateTokenI' constraint which gives access
-- to protocol state.
--
-- Used where we don't know statically what the state type is, but need the
-- agency and message to match each other.
--
data AnyMessage ps (f :: ps -> Type) where
  AnyMessage :: forall ps f (st :: ps) (st' :: ps).
                ( StateTokenI st
                , ActiveState st
                )
             => f st
             -- ^ local state
             -> Message ps (st :: ps) (st' :: ps)
             -- ^ protocol messsage
             -> AnyMessage ps f


-- | `showAnyMessage` is can be used to provide `Show` instance for
-- `AnyMessage` if showing `Message` is independent of the state or one accepts
-- showing only partial information included in message constructors or accepts
-- message constructors to carry `Show` instances for its arguments.  Note that
-- the proper solution is to define a custom `Show (AnyMessage ps f)` instance
-- for a protocol `ps`, which give access to the state functor `f` in scope of
-- `show`.
--
showAnyMessage :: forall ps f.
                  ( forall st st'. Show (Message ps st st')
                  , forall st. Show (f st)
                  )
               => AnyMessage ps f
               -> String
showAnyMessage (AnyMessage st msg) =
    concat [ "AnyMessage "
           , show st
           , " "
           , show msg
           ]


-- | A convenient pattern synonym which unwrap 'AnyMessage' giving both the
-- singleton for the state and the message.
--
pattern AnyMessageAndAgency :: forall ps f. ()
                            => forall (st :: ps) (st' :: ps).
                               (StateTokenI st, ActiveState st)
                            => StateToken st
                            -> f st
                            -> Message ps st st'
                            -> AnyMessage ps f
pattern AnyMessageAndAgency stateToken f msg <- AnyMessage f (getAgency -> (msg, stateToken))
  where
    AnyMessageAndAgency _ msg = AnyMessage msg
{-# COMPLETE AnyMessageAndAgency #-}

-- | Internal view pattern for 'AnyMessageAndAgency'
--
getAgency :: StateTokenI st => Message ps st st' -> (Message ps st st', StateToken st)
getAgency msg = (msg, stateToken)

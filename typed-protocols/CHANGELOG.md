# Revision history for typed-protocols

## next version

### Breaking changes

### Non-breaking changes

* Repository moved to https://github.com/IntersectMBO/typed-protocols
* Added `Lookahead`, a dual of `Pipelined`: a peer defers its sends to
  background `Sender`s and receives ahead (`AwaitLookahead` / `FlushSender`),
  with supporting proofs, drivers, and `Server` pattern synonyms.
* Support `QuickCheck >= 2.18`, `io-classes-1.11`.

## 1.2.1.0 -- 2026-04-16

* Support GHC-9.14, io-classes >=1.8 && < 1.11

## 1.2.0.0 -- 2025-02-05

* Make `runPeerWithDriver` strict, it evaluates the result and `dstate` to
  normal form.

## 1.1.0.1 -- 2025-10-14

* Support QuickCheck <= 2.15

## 1.1.0.0 -- 2025-08-03

### Breaking changes

* Annotated codecs which allow to retain original bytes received from the network.
  The `Codec` type evolved into a new `CodecF` data type, and two type aliases
  `AnnotatedCodec`, `Codec`.
* `prop_codec` properties moved to `typed-protocols:codec-properties` library
  (`Network.TypedProtocol.Codec.Properties` module).  They now return the
  `QuickCheck`'s `Property` rather than a `Bool`.

### Non-breaking changes

## 1.0.0.0

* Hackage release.

## 0.3.0.0

* `AnyMessageWithAgency` pattern synonym is exported as a constructor of `AnyMessage`.
* Bumped version to agree with `typed-protocols-stateful`.

## 0.2.0.0

* A major redesign of `typed-protocols`.
  * `Protocol` class does not require to provide proof obligations for agency.
    Proofs are now provided by the framework for all protocols. Agency is now
    provided by an associated type family `StateAgency`, and evidence for it,
    in form of a singleton, by `StateToken` type family
    (similar to `Sing` from the `singletons` package).
  * `Peer` takes a different evidence type for agency,
    `Network.TypedProtocol.Peer.{Client,Server}` modules provide pattern synonyms
     which provide agency evidence and thus are easier to use.
  * One `Peer` is provided for both non- and pipelined protocol evolution.
    An extra parameter is added of kind `IsPipelined`. As a result
    `Outstanding` is now a type family rather than a type alias.
  * `ActiveAgency` type class is used to distinguish states in which one side
    has an agency (e.g. the protocol hasn't yet terminated), `nonActiveState` can
    be used in the same way as `Data.Void.absurd` - which is useful when writing
    codecs.

## 0.1.1.1
* unbuildable (with `base < 0` constraint in CHaP); We cannot support
`io-classes-1.{6,7}` until `Haskell.Nix` support for public sublibraries is
 merged.

## 0.1.0.7 -- 2023-10-20

* Improved performance of `prop_codecs_splitsM` and `prop_codecs_compatM`.

## 0.1.0.5 -- 2023-03-08

* Support `ghc-9.6.1`.
* Use `io-classes-1.1.0.0`.

[singletons-3.0.1]: https://hackage.haskell.org/package/singletons 

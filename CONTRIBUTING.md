# Building

The project is build with `cabal-install`.  You might need to run `cabal
update` after cloning the repository (to update [`Cardano Haskell
Packages`][CHaP] (`ChaP`) index).

# Using in your project

Currently the package is published to [`CHaP`][CHaP].  In future it will be
published to `Hackage`.  If you want to pull it from [`CHaP`][CHaP], this is
relatively easy to setup; for example, checkout the
[`cabal.project`](https://github.com/intersectmbo/typed-protocols/blob/master/cabal.project)
file.  Alternatively, you can relay on `source-repository-package` stanza in
a `cabal.project` file.

# Testing

The `typed-protocols-examples` package contains two simple protocols `PingPong`
& `ReqResp` which are used for illustration & testing.

```
cabal run typed-protocols-examples:test
```

# Code Style

Please follow local style.  For a more detailed style guide see
[link](https://github.com/intersectmbo/ouroboros-network/blob/master/docs/StyleGuide.md).

# Pull Requests

Each commit shall be small and preferably address one thing at a time.  Well
organised & documented commits make it much easier for the maintainers to
review them.

New features should be well documented & tested, which means including new
tests as necessary.  You might be asked by the maintainers to write & include
additional tests.

Each commit should build & test, at least the package you are changing.  You
can update other packages from this repository in a subsequent commit.

Please use a draft PRs if the work is still in progress.

We require all commits to be signed, see [this guide][gh-signing-commits].

If your pull requests resolves an existing issue, please link your PR to that
issue, see [GitHub documentation][gh-link-issue].


[CHaP]: https://github.com/intersectmbo/cardano-haskell-packages/
[gh-link-issue]: https://docs.github.com/en/github/managing-your-work-on-github/linking-a-pull-request-to-an-issue
[gh-signing-commits]: https://docs.github.com/en/authentication/managing-commit-signature-verification/signing-commits

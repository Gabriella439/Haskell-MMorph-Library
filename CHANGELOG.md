1.2.0

* BREAKING CHANGE: Remove instances for `ErrorT` and `ListT`
  * These types are deprecated

1.1.5

* Fix build failures on GHC 8.4 and 8.6
* Add support for safe Haskell
    * Specifically, this marks the `Control.Monad.Trans.Compose` module as
      `Trustworthy`
    * The change in 1.1.4 to use `GeneralizedNewtypeDeriving` meant that the
      `Control.Monad.Trans.Compose` module was no longer inferred as safe
* Restore `Traversable` instance removed by mistake in 1.1.4

1.1.4 (Blacklisted)

* Unintentional removal of `Traversable` instance for `ComposeT`
    * This missing instance is restored in 1.1.5
    * This is the reason why the 1.1.4 release is blacklisted
* Fix `MonadFail`-related code to work for GHCJS
* The `MonadRWS` instance for `ComposeT` has a more flexible constraint
    * The constraint is now
      `MonadReader r (f (g m)), MonadWriter w (f (g m)), MonadState s (f (g m))`
      instead of `MonadRWS r w s (f g m)`
    * This loosening of the constraint is backwards-compatible

1.1.3

* Add an `MFunctor` instance for `ComposeT` for GHC >= 8.6
* Add GHC 8.8 support

1.1.2

* Conditionally disable `Polykinds` to support older versions of GHC

1.1.1

* Increase upper bound on `transformers-compat`

1.1.0

* BREAKING CHANGE: Enable `PolyKinds`
    * This should in theory be a non-breaking change, but due to a bug in
      GHC 8.0.1 and kind inference ambiguities I'm marking this as a breaking
      change

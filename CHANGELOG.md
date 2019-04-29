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

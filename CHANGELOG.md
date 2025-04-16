1.0.1.2
-------

* Fix dependency bounds (including retroactively), and compilation with warnings.

1.0.1.1
-------

* Fix compilation with GHC 9.8.3

1.0.1
-----

* Compillability by GHC-9.2.4
* Fix cabal `tested-with` field

1.0.0
-----

* Fixed issue with too big terms in case of recursive types
* Recpect the `size` argument
* Types with parameters require `Arg` now

0.2.2
-----

* Resolved an issue where the size of the generators could become negative

0.2.1
-----

* Added compatibility with GHC 9.2.1

0.2.0
-----

* Added `GenericArbitrary` for use with the [DerivingVia](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html) compiler extension.
* Minimum bound on QuickCheck changed to 2.14.

0.1.0
-----

* Public release

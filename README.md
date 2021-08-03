# generic-arbitrary

[![Build Status](https://travis-ci.org/typeable/generic-arbitrary.svg?branch=master)](https://travis-ci.org/typeable/generic-arbitrary)

Deriving `Arbitrary` via `Generic`.

``` haskell
import           GHC.Generics                      (Generic)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic

data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show, Generic)

instance Arbitrary Expr where
  arbitrary = genericArbitrary
  shrink = genericShrink
```

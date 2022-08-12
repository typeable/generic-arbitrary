# generic-arbitrary

[![GitHub CI](https://github.com/typeable/generic-arbitrary/workflows/haskell-ci/badge.svg)](https://github.com/typeable/generic-arbitrary/actions)

# What?

Package for deriving `Arbitrary` via `Generic`.

``` haskell
import           GHC.Generics                      (Generic)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic

data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show, Generic)
  deriving Arbitrary via (GenericArbitrary Expr)
```

Older versions of this package had a problem with hanging `arbitrary`
method. Since `1.0.0` this problem almost solved.

For `QuickCheck` older than `2.14.0` the `GenericArbitrary` is not available, so
you will need to write instances more verbosely

``` haskell
data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show, Generic)

instance Arbitrary Expr where
  arbitrary = genericArbitrary
  shrink = genericShrink
```

Which is generally the same.

# Infinite terms problem

The `generic-arbitrary` can partially handle the problem with recursive
types. Assume the type `R`

``` haskell
data R = R R
  deriving Generic
```

there is no instance

``` haskell
instance Arbitrary R where
  arbitrary = genericArbitrary
  shrink = genericShrink
```

If you try to compile this you will get a type level error

>    • R refers to itself in all constructors

Which means that there is no finite term for `R` because it is recursive in all
it's constructors. But, if you correct the definition of `R` like this.

``` haskell
data R = R R | F
  deriving Generic
```

Then it will compile. And the `arbitrary` generated will not hang forever,
because it respects the `size` parameter.

## Limitation

There is a limitation of recursion detection:

``` haskell
data R1 = R1 R2
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass NFData
  deriving Arbitrary via (GenericArbitrary R1)

data R2 = R2 R1
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass NFData
  deriving Arbitrary via (GenericArbitrary R2)
```

This code will compile and the `arbitrary` generated will always hang. Yes,
there is a problem with mutually recursive types.

# Type parameters

Now lets see an example of datatype with parameters

``` haskell
data A a = A a
  deriving (Eq, Ord, Show)
  deriving anyclass NFData
  deriving (Generic)

instance (Arbitrary a) => Arbitrary (A a) where
  arbitrary = genericArbitrary
  shrink = genericShrink
```

It should work from first glance, but when compile it will throw an error:

> • Could not deduce (Test.QuickCheck.Arbitrary.Generic.GArbitrary
>                           (A a)
>                           (GHC.Generics.D1
>                              ('GHC.Generics.MetaData "A" "ParametersTest" "main" 'False)
>                              (GHC.Generics.C1
>                                 ('GHC.Generics.MetaCons "A" 'GHC.Generics.PrefixI 'False)
>                                 (GHC.Generics.S1
>                                    ('GHC.Generics.MetaSel
>                                       'Nothing
>                                       'GHC.Generics.NoSourceUnpackedness
>                                       'GHC.Generics.NoSourceStrictness
>                                       'GHC.Generics.DecidedLazy)
>                                    (GHC.Generics.Rec0 a))))
>                           (TypesDiffer (A a) a))
>         arising from a use of ‘genericArbitrary’

Here the `TypesDiffer` is a type familty dealing with recursive types and
helping us to eliminate inproper instances. To convince the compiller, that the
`a` parameter is not an `A a` we must fix the instance with additional constraint

``` haskell
instance (Arg (A a) a, Arbitrary a) => Arbitrary (A a) where
  arbitrary = genericArbitrary
  shrink = genericShrink
```

Now everything compiles and works as expected.

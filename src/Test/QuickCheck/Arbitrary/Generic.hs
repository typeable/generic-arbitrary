#if __GLASGOW_HASKELL__ >= 806
{-# OPTIONS_GHC -Wno-star-is-type #-}
#endif
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
#endif

{-|

This module is a generic implementation of the 'arbitrary' method. Example
usage:

@
data Foo = Foo
  { _fooX :: X
  , _fooY :: Y
  } deriving (Generic)

instance Arbitrary Foo where
  arbitrary = genericArbitrary
  shrink = genericShrink
@

This instance can also be derived using DerivingVia language extension

@
data Foo = Foo
  { _fooX :: X
  , _fooY :: Y
  } deriving (Generic)
    deriving (Arbitrary) via GenericArbitrary Foo
@

The generated 'arbitrary' method is equivalent to

@
Foo '<$>' arbitrary '<*>' arbitrary
@.

It can also handle a recursive types problem. Assuming a type

@
data R = R R
  deriving Generic
@

there is no instance

@
instance Arbitrary R where
  arbitrary = genericArbitrary
  shrink = genericShrink
@

If you try to compile this you will get a type level error

>    • R refers to itself in all constructors

Which means that there is no finite term for @R@ because it is recursive. But,
if you correct the definition of @R@ like this.

@
data R = R R | F
  deriving Generic
@

Then it will compile. And the @arbitrary@ generated will not hang forever, because
it respects the @size@ parameter.

There is a limitation of recursion detection:

@
data R1 = R1 R2
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass NFData
  deriving Arbitrary via (GenericArbitrary R1)

data R2 = R2 R1
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass NFData
  deriving Arbitrary via (GenericArbitrary R2)
@

This code will compile and the @arbitrary@ generated will always hang. Yes,
there is a problem with mutually recursive types.

Now lets see an example of datatype with parameters

@
data A a = A a
  deriving (Eq, Ord, Show)
  deriving anyclass NFData
  deriving (Generic)

instance (Arbitrary a) => Arbitrary (A a) where
  arbitrary = genericArbitrary
  shrink = genericShrink
@

It should work from first glance, but when compile it will throw an error:

@
    • Could not deduce (Test.QuickCheck.Arbitrary.Generic.GArbitrary
                          (A a)
                          (GHC.Generics.D1
                             ('GHC.Generics.MetaData "A" "ParametersTest" "main" 'False)
                             (GHC.Generics.C1
                                ('GHC.Generics.MetaCons "A" 'GHC.Generics.PrefixI 'False)
                                (GHC.Generics.S1
                                   ('GHC.Generics.MetaSel
                                      'Nothing
                                      'GHC.Generics.NoSourceUnpackedness
                                      'GHC.Generics.NoSourceStrictness
                                      'GHC.Generics.DecidedLazy)
                                   (GHC.Generics.Rec0 a))))
                          (TypesDiffer (A a) a))
        arising from a use of ‘genericArbitrary’
@

Here the @TypesDiffer@ is a type familty dealing with recursive types and
helping us to eliminate inproper instances. To convince the compiller, that the
@a@ parameter is not an @A a@ we must fix the instance with additional constraint

@
instance (Arg (A a) a, Arbitrary a) => Arbitrary (A a) where
  arbitrary = genericArbitrary
  shrink = genericShrink
@

Now everything compiles and works as expected.

-}

module Test.QuickCheck.Arbitrary.Generic
  ( -- * Main
    genericArbitrary
#if MIN_VERSION_QuickCheck(2, 14, 0)
  , GenericArbitrary(..)
#endif
  , Arg
  -- * Internal
  , GArbitrary
  , FiniteSum
  , FiniteSumElem
  , Finite
  , AllFieldsFinal
  , TypesDiffer
  , ArgumentsCount
  , SumLen
  -- * Reexports
  , Arbitrary(..)
  , genericShrink
  ) where

import           Control.Applicative
import           Data.Proxy
import           Data.Type.Bool
import           GHC.Generics              as G
import           GHC.TypeLits
import           Prelude
import           Test.QuickCheck           as QC
#if MIN_VERSION_QuickCheck(2, 14, 0)
import           Data.Coerce (coerce)
import           Test.QuickCheck.Arbitrary (GSubterms, RecursivelyShrink)



-- | Newtype for @DerivingVia@
--
-- Usage:
--
-- @
-- data Foo = Foo
--   { _fooX :: X
--   , _fooY :: Y
--   } deriving (Generic)
--     deriving (Arbitrary) via GenericArbitrary Foo
-- @
--
-- @since 1.0.0
newtype GenericArbitrary a = GenericArbitrary { unGenericArbitrary :: a }
  deriving (Show, Eq)

instance
  ( Generic a,
    GArbitrary a (Rep a) some,
    RecursivelyShrink (Rep a),
    GSubterms (Rep a) a
  ) => Arbitrary (GenericArbitrary a) where
  arbitrary = coerce (genericArbitrary :: Gen a)
  shrink = coerce (genericShrink :: a -> [a])
#endif

-- | Constraint helper for types with parameters
--
-- Usage:
--
-- @
-- data A a = A a
--   deriving (Generic)
-- instance (Arg (A a) a, Arbitrary a) => Arbitrary (A a) where
--   arbitrary = genericArbitrary
--   shrink = genericShrink
-- @
--
-- @since 1.0.0

type Arg self field = (TypesDiffer self field ~ 'True)

type family TypesDiffer a b where
  TypesDiffer a a = 'False
  TypesDiffer a b = 'True

type family AllFieldsFinal self (a :: * -> *) :: Bool where
  AllFieldsFinal self U1 = 'True
  AllFieldsFinal self (a :*: b) = AllFieldsFinal self a && AllFieldsFinal self b
  AllFieldsFinal self (M1 S t (K1 R field)) = TypesDiffer self field

type family Finite self (a :: * -> *) :: Bool where
  Finite self U1 = 'True
  Finite self (K1 R field) = TypesDiffer self field
  Finite self (a :*: b) = Finite self a && Finite self b
  Finite self (M1 D t f) = Finite self f
  Finite self (a :+: b) = Finite self a || Finite self b
  Finite self (M1 C c f) = AllFieldsFinal self f
  Finite self (M1 S s f) = Finite self f

type family ArgumentsCount (a :: * -> *) :: Nat where
  ArgumentsCount U1 = 1
  ArgumentsCount (M1 S s f) = 1
  ArgumentsCount (a :*: b) = (ArgumentsCount a) + (ArgumentsCount b)

-- | Calculates count of constructors encoded by particular ':+:'.
-- Internal use only.
type family SumLen a :: Nat where
  SumLen (a G.:+: b) = (SumLen a) + (SumLen b)
  SumLen a           = 1

-- | Generic arbitrary.
--
-- Parameters are:
-- * self: the ADT we generating instance for
-- * a: some part of the `Rep self`
-- * finite: Is `a` finite? Infinite type has no finite values (like Stream)
class (Finite self a ~ finite) => GArbitrary self a (finite :: Bool) where
  gArbitrary :: Proxy self -> QC.Gen (a x)

instance
  ( GArbitrary self (M1 C c f) 'True
  ) => GArbitrary self (M1 D t (M1 C c f)) 'True where
  gArbitrary _ = M1 <$> gArbitrary (Proxy :: Proxy self)

-- | The constructor meta information
instance
  ( GArbitrary self f some
  , KnownNat (ArgumentsCount f)
  , AllFieldsFinal self f ~ some
  ) => GArbitrary self (M1 C c f) some where
  gArbitrary _ = M1 <$> scale predNat (gArbitrary (Proxy :: Proxy self))
    where
      argumentsCount = fromIntegral $ natVal (Proxy :: Proxy (ArgumentsCount f))
      predNat n = max 0 $ if argumentsCount > 1
        then n `div` argumentsCount
        else pred n

-- | Unit type instance
instance GArbitrary self U1 'True where
  gArbitrary _ = pure U1

-- | Constructor field meta information
instance GArbitrary self f some => GArbitrary self (M1 S t f) some where
  gArbitrary _ = M1 <$> gArbitrary (Proxy :: Proxy self)

-- | Data of the constructor field
instance
  ( Arbitrary t
  , Finite self (K1 R t) ~ some
  ) => GArbitrary self (K1 R t) some where
  gArbitrary _ = K1 <$> arbitrary

-- | Product
instance
  ( GArbitrary self a af
  , GArbitrary self b bf
  , (af && bf) ~ some
  ) => GArbitrary self (a :*: b) some where
  gArbitrary _ = liftA2 (:*:)
    (gArbitrary (Proxy :: Proxy self)) (gArbitrary (Proxy :: Proxy self))

#if __GLASGOW_HASKELL__ >= 800
instance
  ( TypeError (ShowType self :<>: Text " refers to itself in all constructors")
  , AllFieldsFinal self f ~ 'False
  ) => GArbitrary self (M1 D t (M1 C c f)) 'False where
  gArbitrary _ = error "Unreachable"
#endif

-- | ADT declaration with multiple constructors
instance
  ( FiniteSum self a b af bf
  , GArbitrary self (a :+: b) 'True
  ) => GArbitrary self (M1 D t (a :+: b)) 'True where
  gArbitrary _ = sized $ \s -> M1 <$>
    if s > 1
    then gArbitrary (Proxy :: Proxy self)
    else oneof (finiteSum (Proxy :: Proxy self))

-- | Any sum inside of declaration
instance
  ( GArbitrary self a af, GArbitrary self b bf
  , KnownNat (SumLen a), KnownNat (SumLen b)
  , (af || bf) ~ some
  ) => GArbitrary self (a :+: b) some where
  gArbitrary _ = frequency
    [ (lfreq, G.L1 <$> gArbitrary (Proxy :: Proxy self))
    , (rfreq, G.R1 <$> gArbitrary (Proxy :: Proxy self)) ]
    where
      lfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen a))
      rfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen b))

class
  ( Finite self a ~ af, Finite self b ~ bf
  ) => FiniteSum self (a :: * -> *) (b :: * -> *) af bf where
  finiteSum :: Proxy self -> [Gen ((a :+: b) p)]

instance
  ( FiniteSumElem self a, FiniteSumElem self b
  , Finite self a ~ 'True
  , Finite self b ~ 'True
  ) => FiniteSum self a b 'True 'True where
  finiteSum _ = concat
    [ fmap L1 <$> finiteElem (Proxy :: Proxy self)
    , fmap R1 <$> finiteElem (Proxy :: Proxy self)]

instance
  ( FiniteSumElem self a
  , Finite self a ~ 'True
  , Finite self b ~ 'False
  ) => FiniteSum self a b 'True 'False where
  finiteSum _ = fmap L1 <$> finiteElem (Proxy :: Proxy self)

instance
  ( FiniteSumElem self b
  , Finite self a ~ 'False
  , Finite self b ~ 'True
  ) => FiniteSum self a b 'False 'True where
  finiteSum _ = fmap R1 <$> finiteElem (Proxy :: Proxy self)

class FiniteSumElem self a where
  finiteElem :: Proxy self -> [Gen (a p)]

instance
  ( FiniteSum self a b af bf
  ) => FiniteSumElem self (a :+: b) where
  finiteElem _ = finiteSum (Proxy :: Proxy self)

instance
  ( GArbitrary self (M1 C c f) 'True
  ) => FiniteSumElem self (M1 C c f) where
  finiteElem _ = [gArbitrary (Proxy :: Proxy self)]


#if __GLASGOW_HASKELL__ >= 800
instance
  ( TypeError (ShowType self :<>: Text " refers to itself in all constructors")
  , (Finite self a || Finite self b) ~ 'False
  ) => GArbitrary self (M1 D t (a :+: b)) 'False where
  gArbitrary _ = error "Unreachable"
#endif

genericArbitrary
  :: forall a ga some
  . (Generic a, GArbitrary a ga some, ga ~ Rep a)
  => Gen a
genericArbitrary = G.to <$> gArbitrary (Proxy :: Proxy a)

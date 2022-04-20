{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeOperators, DataKinds, TypeFamilies, ScopedTypeVariables#-}

{- |

Generic implementation of the 'arbitrary' method. Example usage:

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

@Foo <$> arbitrary <*> arbitrary@.

-}

module Test.QuickCheck.Arbitrary.Generic
  ( GenericArbitrary(..)
  , Arbitrary(..)
  , genericArbitrary
  , genericShrink
  ) where

import Control.Applicative
import Data.Coerce (coerce)
import Data.Proxy
import GHC.Generics as G
import GHC.TypeLits
import Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary (GSubterms, RecursivelyShrink)

newtype GenericArbitrary a = GenericArbitrary { unGenericArbitrary :: a }
  deriving (Show, Eq)

instance
  ( Generic a,
    GArbitrary (Rep a),
    RecursivelyShrink (Rep a),
    GSubterms (Rep a) a
  ) => Arbitrary (GenericArbitrary a) where
  arbitrary = coerce (genericArbitrary :: Gen a)
  shrink = coerce (genericShrink :: a -> [a])

class GArbitrary a where
  gArbitrary :: QC.Gen (a x)

instance GArbitrary G.U1 where
  gArbitrary = pure G.U1

instance Arbitrary c => GArbitrary (G.K1 i c) where
  gArbitrary = G.K1 <$> arbitrary

instance GArbitrary f => GArbitrary (G.M1 i c f) where
  gArbitrary = G.M1 <$> gArbitrary

instance (GArbitrary a, GArbitrary b) => GArbitrary (a G.:*: b) where
  gArbitrary = liftA2 (G.:*:) gArbitrary gArbitrary

-- | Calculates count of constructors encoded by particular ':+:'.
-- Internal use only.
type family SumLen a :: Nat where
  SumLen (a G.:+: b) = (SumLen a) + (SumLen b)
  SumLen a           = 1

instance (GArbitrary a, GArbitrary b, KnownNat (SumLen a), KnownNat (SumLen b)
         ) => GArbitrary (a G.:+: b) where
  gArbitrary = frequency
    [ (lfreq, G.L1 <$> QC.scale (max 0 . pred) gArbitrary)
    , (rfreq, G.R1 <$> QC.scale (max 0 . pred) gArbitrary) ]
    where
      lfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen a))
      rfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen b))

genericArbitrary :: (Generic a, GArbitrary ga, ga ~ G.Rep a) => Gen a
genericArbitrary = G.to <$> gArbitrary

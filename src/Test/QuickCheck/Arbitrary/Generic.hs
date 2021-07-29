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

import           Control.Applicative
import           Data.Coerce               (coerce)
import           Data.Proxy
import           Data.Type.Bool
import           GHC.Generics              as G
import           GHC.TypeLits
import           Test.QuickCheck           as QC
import           Test.QuickCheck.Arbitrary (GSubterms, RecursivelyShrink)
import           TypeFun.Data.Eq
import           TypeFun.Data.List

newtype GenericArbitrary a = GenericArbitrary { unGenericArbitrary :: a }
  deriving (Show, Eq)

instance
  ( Generic a,
    GArbitrary (Rep a) (Rep a),
    RecursivelyShrink (Rep a),
    GSubterms (Rep a) a
  ) => Arbitrary (GenericArbitrary a) where
  arbitrary = coerce (genericArbitrary :: Gen a)
  shrink = coerce (genericShrink :: a -> [a])

-- | Class of finite types
class Finite a where
  -- | Generates final element, when size = 0.
  final :: QC.Gen a

class GArbitrary root a where
  gArbitrary :: Proxy root -> QC.Gen (a x)

-- | Unit type instance
instance GArbitrary root G.U1 where
  gArbitrary _ = pure G.U1

-- | Data of the constructor field
instance Arbitrary c => GArbitrary root (Rec0 c) where
  gArbitrary _ = G.K1 <$> arbitrary

-- | Data type meta information
instance GArbitrary (D1 t f) f => GArbitrary (D1 t f) (D1 t f) where
  gArbitrary _ = G.M1 <$> gArbitrary (Proxy @(D1 t f))

-- | The constructor meta information
instance GArbitrary root f => GArbitrary root (C1 c f) where
  gArbitrary _ = G.M1 <$> scale predNat (gArbitrary (Proxy @root))
    where
      predNat 0 = 0
      predNat n = pred n

-- | Constructor field meta information
instance GArbitrary root f => GArbitrary root (S1 t f) where
  gArbitrary _ = G.M1 <$> gArbitrary (Proxy @root)

-- | Product
instance (GArbitrary root a, GArbitrary root b) => GArbitrary root (a :*: b) where
  gArbitrary _ = liftA2 (:*:) (gArbitrary (Proxy @root)) (gArbitrary (Proxy @root))

-- | Calculates count of constructors encoded by particular ':+:'.
-- Internal use only.
type family SumLen a :: Nat where
  SumLen (a G.:+: b) = (SumLen a) + (SumLen b)
  SumLen a           = 1

type family FinalConstructors root (a :: * -> *) :: [* -> *] where
  FinalConstructors root (a :+: b) =
    FinalConstructors root a :++: FinalConstructors root b
  FinalConstructors root (M1 C t f) = If (AllFieldsFinal root f) '[M1 C t f] '[]

type family AllFieldsFinal root (a :: * -> *) :: Bool where
  AllFieldsFinal root U1 = 'True
  AllFieldsFinal root (a :*: b) = AllFieldsFinal root a && AllFieldsFinal root b
  AllFieldsFinal root (M1 S t (K1 R typ)) = Not (Equal (Rep typ) root)

-- | Sum
instance ( (FinalConstructors root (a :+: b) ~ (x ': y))
         , GArbitrary root a, GArbitrary root b
         , KnownNat (SumLen a), KnownNat (SumLen b)
         ) => GArbitrary root (a :+: b) where
  gArbitrary _ = do
    sized $ \s ->
      if s > 1 then usual else final
    where
      usual = frequency
        [ (lfreq, G.L1 <$> (gArbitrary (Proxy @root)))
        , (rfreq, G.R1 <$> (gArbitrary (Proxy @root))) ]
        where
          lfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen a))
          rfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen b))
      final = (Prelude.error "FIXME: not implemented")

genericArbitrary
  :: forall a ga
  . (Generic a, GArbitrary ga ga, ga ~ G.Rep a)
  => Gen a
genericArbitrary = G.to <$> gArbitrary (Proxy @ga)

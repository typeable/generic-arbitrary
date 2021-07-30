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
    GArbitrary a (Rep a),
    RecursivelyShrink (Rep a),
    GSubterms (Rep a) a
  ) => Arbitrary (GenericArbitrary a) where
  arbitrary = coerce (genericArbitrary :: Gen a)
  shrink = coerce (genericShrink :: a -> [a])

-- | Class of finite types
class Finite a where
  -- | Generates final element, when size = 0.
  final :: QC.Gen a

class GArbitrary self a where
  gArbitrary :: Proxy self -> QC.Gen (a x)

-- | Unit type instance
instance GArbitrary self G.U1 where
  gArbitrary _ = pure G.U1

-- | Data of the constructor field
instance Arbitrary c => GArbitrary self (Rec0 c) where
  gArbitrary _ = G.K1 <$> arbitrary

-- | Data type with single constructor
instance
  ( AllFieldsFinal self f ~ 'True
  , GArbitrary self (C1 c f)
  ) => GArbitrary self (D1 t (C1 c f)) where
  gArbitrary _ = G.M1 <$> gArbitrary (Proxy @self)

-- | Data type with multiple constructors
instance
  ( FinalConstructors self (a :+: b) ~ (x ': y)
  , GArbitrary self (a :+: b)
  ) => GArbitrary self (D1 t (a :+: b)) where
  gArbitrary _ = G.M1 <$> gArbitrary (Proxy @self)

-- | The constructor meta information
instance GArbitrary self f => GArbitrary self (C1 c f) where
  gArbitrary _ = G.M1 <$> scale predNat (gArbitrary (Proxy @self))
    where
      predNat 0 = 0
      predNat n = pred n

-- | Constructor field meta information
instance GArbitrary self f => GArbitrary self (S1 t f) where
  gArbitrary _ = G.M1 <$> gArbitrary (Proxy @self)

-- | Product
instance (GArbitrary self a, GArbitrary self b) => GArbitrary self (a :*: b) where
  gArbitrary _ = liftA2 (:*:) (gArbitrary (Proxy @self)) (gArbitrary (Proxy @self))

-- | Calculates count of constructors encoded by particular ':+:'.
-- Internal use only.
type family SumLen a :: Nat where
  SumLen (a G.:+: b) = (SumLen a) + (SumLen b)
  SumLen a           = 1

type family FinalConstructors self (a :: * -> *) :: [* -> *] where
  FinalConstructors self (a :+: b) =
    FinalConstructors self a :++: FinalConstructors self b
  FinalConstructors self (M1 C t f) = If (AllFieldsFinal self f) '[M1 C t f] '[]

type family AllFieldsFinal self (a :: * -> *) :: Bool where
  AllFieldsFinal self U1 = 'True
  AllFieldsFinal self (a :*: b) = AllFieldsFinal self a && AllFieldsFinal self b
  AllFieldsFinal self (M1 S t (K1 R self)) = 'False
  AllFieldsFinal self (M1 S t (K1 R other)) = 'True

-- | Sum
instance ( GArbitrary self a, GArbitrary self b
         , KnownNat (SumLen a), KnownNat (SumLen b)
         ) => GArbitrary self (a :+: b) where
  gArbitrary _ = do
    sized $ \s ->
      if s > 1 then usual else final
    where
      usual = frequency
        [ (lfreq, G.L1 <$> (gArbitrary (Proxy @self)))
        , (rfreq, G.R1 <$> (gArbitrary (Proxy @self))) ]
        where
          lfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen a))
          rfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen b))
      final = (Prelude.error "FIXME: not implemented")

genericArbitrary
  :: forall a ga
  . (Generic a, GArbitrary a ga, ga ~ G.Rep a)
  => Gen a
genericArbitrary = G.to <$> gArbitrary (Proxy @a)

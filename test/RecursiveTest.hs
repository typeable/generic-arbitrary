{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=5 #-} -- GHC-9.2.4

-- | Testing that our Arbitrary instances do not get stuck and respect the
-- `size` parameter while generating.

module RecursiveTest where

import           Control.DeepSeq hiding (Unit)
import           GHC.Generics                      (Generic)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic

data Unit = Unit
  deriving (Eq, Show, Generic)
  deriving anyclass NFData
#if MIN_VERSION_QuickCheck(2, 14, 0)
  deriving Arbitrary via (GenericArbitrary Unit)
#else
instance Arbitrary Unit where
  arbitrary = genericArbitrary
  shrink = genericShrink
#endif

prop_unitTotal :: Unit -> Property
prop_unitTotal = total

data Single = Single Int
  deriving (Eq, Show, Generic)
  deriving anyclass NFData
#if MIN_VERSION_QuickCheck(2, 14, 0)
  deriving Arbitrary via (GenericArbitrary Single)
#else
instance Arbitrary Single where
  arbitrary = genericArbitrary
  shrink = genericShrink
#endif

prop_singleTotal :: Single -> Property
prop_singleTotal = total

data Multiple = M1 | M2 | M3 Int
  deriving (Eq, Show, Generic)
  deriving anyclass NFData
#if MIN_VERSION_QuickCheck(2, 14, 0)
  deriving Arbitrary via (GenericArbitrary Multiple)
#else
instance Arbitrary Multiple where
  arbitrary = genericArbitrary
  shrink = genericShrink
#endif

prop_multipleTotal :: Multiple -> Property
prop_multipleTotal = total

data Rec = Rec
  { unit     :: Unit
  , single   :: Single
  , multiple :: Multiple
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData
#if MIN_VERSION_QuickCheck(2, 14, 0)
  deriving Arbitrary via (GenericArbitrary Rec)
#else
instance Arbitrary Rec where
  arbitrary = genericArbitrary
  shrink = genericShrink
#endif

prop_recTotal :: Rec -> Property
prop_recTotal = total

data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show, Generic)
  deriving anyclass NFData
#if MIN_VERSION_QuickCheck(2, 14, 0)
  deriving Arbitrary via (GenericArbitrary Expr)
#else
instance Arbitrary Expr where
  arbitrary = genericArbitrary
  shrink = genericShrink
#endif

prop_exprTotal :: Expr -> Property
prop_exprTotal = total

data Recursive
  = R1 Recursive
  | R2 Recursive Int
  | N Int
  | R3 Recursive
  | R4 Recursive
  | R5 Recursive String
  deriving (Eq, Show, Generic)
  deriving anyclass NFData
#if MIN_VERSION_QuickCheck(2, 14, 0)
  deriving Arbitrary via (GenericArbitrary Recursive)
#else
instance Arbitrary Recursive where
  arbitrary = genericArbitrary
  shrink = genericShrink
#endif

prop_recursiveTotal :: Recursive -> Property
prop_recursiveTotal = total

data DeepRecursive
  = Deep DeepRecursive DeepRecursive DeepRecursive DeepRecursive DeepRecursive
  | Short
  deriving (Eq, Show, Generic)
  deriving anyclass NFData
#if MIN_VERSION_QuickCheck(2, 14, 0)
  deriving Arbitrary via (GenericArbitrary DeepRecursive)
#else
instance Arbitrary DeepRecursive where
  arbitrary = genericArbitrary
  shrink = genericShrink
#endif

prop_deepRecursiveTotal :: DeepRecursive -> Property
prop_deepRecursiveTotal = total

data P p = NoP | P p
  deriving (Eq, Show, Generic)
  deriving anyclass NFData

-- Note that deriving via doesn't work for case with arguments
instance (Arg (P p) p, Arbitrary p) => Arbitrary (P p) where
  arbitrary = genericArbitrary
  shrink = genericShrink

prop_Ptotal :: P (P Int) -> Property
prop_Ptotal = total

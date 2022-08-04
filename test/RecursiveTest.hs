{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Testing that our Arbitrary instances do not get stuck and respect the
-- `size` parameter while generating.

module RecursiveTest where

import           Control.DeepSeq
import           GHC.Generics                      (Generic)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic

data Unit = Unit
  deriving (Eq, Show, Generic)
  deriving anyclass NFData
  deriving Arbitrary via (GenericArbitrary Unit)

prop_unitTotal :: Unit -> Property
prop_unitTotal = total

data Single = Single Int
  deriving (Eq, Show, Generic)
  deriving anyclass NFData
  deriving Arbitrary via (GenericArbitrary Single)

prop_singleTotal :: Single -> Property
prop_singleTotal = total

data Multiple = M1 | M2 | M3 Int
  deriving (Eq, Show, Generic)
  deriving anyclass NFData
  deriving Arbitrary via (GenericArbitrary Multiple)

prop_multipleTotal :: Multiple -> Property
prop_multipleTotal = total

data Rec = Rec
  { unit     :: Unit
  , single   :: Single
  , multiple :: Multiple
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData
    deriving Arbitrary via (GenericArbitrary Rec)

prop_recTotal :: Rec -> Property
prop_recTotal = total

data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show, Generic)
  deriving anyclass NFData
  deriving Arbitrary via (GenericArbitrary Expr)

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
  deriving Arbitrary via (GenericArbitrary Recursive)

prop_recursiveTotal :: Recursive -> Property
prop_recursiveTotal = total

data DeepRecursive
  = Deep DeepRecursive DeepRecursive DeepRecursive DeepRecursive DeepRecursive
  | Short
  deriving (Eq, Show, Generic)
  deriving anyclass NFData
  deriving Arbitrary via (GenericArbitrary DeepRecursive)

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

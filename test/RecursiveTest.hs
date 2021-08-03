-- | Testing that our Arbitrary instances do not get stuck and respect the
-- `size` parameter while generating.

module RecursiveTest where

import           Control.DeepSeq
import           GHC.Generics                      (Generic)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic

data Unit = Unit
  deriving (Eq, Show, Generic)

instance NFData Unit

instance Arbitrary Unit where
  arbitrary = genericArbitrary
  shrink = genericShrink

prop_unitTotal :: Unit -> Property
prop_unitTotal = total

data Single = Single Int
  deriving (Eq, Show, Generic)

instance NFData Single

instance Arbitrary Single where
  arbitrary = genericArbitrary
  shrink = genericShrink

prop_singleTotal :: Single -> Property
prop_singleTotal = total

data Multiple = M1 | M2 | M3 Int
  deriving (Eq, Show, Generic)

instance NFData Multiple

instance Arbitrary Multiple where
  arbitrary = genericArbitrary
  shrink = genericShrink

prop_multipleTotal :: Multiple -> Property
prop_multipleTotal = total

data Rec = Rec
  { unit     :: Unit
  , single   :: Single
  , multiple :: Multiple
  } deriving (Eq, Show, Generic)

instance NFData Rec

instance Arbitrary Rec where
  arbitrary = genericArbitrary
  shrink = genericShrink

prop_recTotal :: Rec -> Property
prop_recTotal = total

data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show, Generic)

-- | Instance with may fall into infinite loop
instance Arbitrary Expr where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance NFData Expr

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

instance NFData Recursive

instance Arbitrary Recursive where
  arbitrary = genericArbitrary
  shrink = genericShrink

prop_recursiveTotal :: Recursive -> Property
prop_recursiveTotal = total

data DeepRecursive
  = Deep DeepRecursive DeepRecursive DeepRecursive DeepRecursive DeepRecursive
  | Short
  deriving (Eq, Show, Generic)

instance NFData DeepRecursive

instance Arbitrary DeepRecursive where
  arbitrary = genericArbitrary
  shrink = genericShrink

prop_deepRecursiveTotal :: DeepRecursive -> Property
prop_deepRecursiveTotal = total

data P p = NoP | P p
  deriving (Eq, Show, Generic)

instance (NFData p) =>  NFData (P p)

instance (Arg (P p) p, Arbitrary p) => Arbitrary (P p) where
  arbitrary = genericArbitrary
  shrink = genericShrink

prop_Ptotal :: P (P Int) -> Property
prop_Ptotal = total

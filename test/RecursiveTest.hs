module RecursiveTest where

import           Control.DeepSeq
import           GHC.Generics                      (Generic)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic

data Expr
  = Lit
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show, Generic)

-- | Instance with may fall into infinite loop
instance Arbitrary Expr where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance NFData Expr

data Single = Single
  deriving (Eq, Show, Generic)

instance Arbitrary Single where
  arbitrary = genericArbitrary
  shrink = genericShrink

data Multiple = M1 | M2
  deriving (Eq, Show, Generic)

instance Arbitrary Multiple where
  arbitrary = genericArbitrary
  shrink = genericShrink

data Fail = Fail Fail
  deriving (Eq, Show, Generic)

instance Arbitrary Fail where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- | Check that we are not generating infinite
prop_exprTotal :: Expr -> Property
prop_exprTotal = total

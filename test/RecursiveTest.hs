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

-- | Check that we are not generating infinite
prop_exprTotal :: Expr -> Property
prop_exprTotal = total

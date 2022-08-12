{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module ParametersTest where

import           Aux
import           Control.DeepSeq
import           GHC.Generics                      (Generic)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic

data A a = A a
  deriving (Eq, Ord, Show)
  deriving anyclass NFData
  deriving (Generic)

instance (Arbitrary a) => Arbitrary (A a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

data B a = B a
  deriving (Eq, Ord, Show)
  deriving anyclass NFData
  deriving (Generic)

instance (Arg (B a) a, Arbitrary a) => Arbitrary (B a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

unit_argumentConstraintFail :: IO ()
unit_argumentConstraintFail = failGeneration @(A Int)

prop_properArgumentConstraint :: B Int -> Property
prop_properArgumentConstraint = total

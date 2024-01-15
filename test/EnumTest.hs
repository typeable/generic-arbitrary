#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -fconstraint-solver-iterations=5 #-}
#endif

-- #elif __GLASGOW_HASKELL__ >= 904
-- {-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}


module EnumTest where

import Control.DeepSeq
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

data Country = PL | GB | RU | RO | CZ | HR | SK | DE | NL | ES | BR
  deriving (Generic, NFData, Show)

instance Arbitrary Country where
  arbitrary = genericArbitrary
  shrink = genericShrink

prop_CountryTest :: Country -> Property
prop_CountryTest = total

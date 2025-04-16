#if __GLASGOW_HASKELL__ >= 806 && __GLASGOW_HASKELL__ <= 900
{-# OPTIONS_GHC -fconstraint-solver-iterations=5 #-}
#elif __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -fconstraint-solver-iterations=6 #-}
#endif



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

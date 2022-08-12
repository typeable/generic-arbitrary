module LimitationDemo where

import           Control.DeepSeq
import           GHC.Generics                      (Generic)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic

-- As you can see, we have this instance generated, but it will definitely lead
-- to generating an infinite term (and the only term). The recursion detection
-- doesn't work here
data R1 = R1 R2
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass NFData
#if MIN_VERSION_QuickCheck(2, 14, 0)
  deriving Arbitrary via (GenericArbitrary R1)
#else
instance Arbitrary R1 where
  arbitrary = genericArbitrary
  shrink = genericShrink
#endif

data R2 = R2 R1
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass NFData
#if MIN_VERSION_QuickCheck(2, 14, 0)
  deriving Arbitrary via (GenericArbitrary R2)
#else
instance Arbitrary R2 where
  arbitrary = genericArbitrary
  shrink = genericShrink
#endif

-- To force the instance realy be generated
usage :: IO ()
usage = do
  r1 :: R1 <- generate arbitrary
  print r1 -- haha

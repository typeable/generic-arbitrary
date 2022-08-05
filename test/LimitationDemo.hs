{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}

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
  deriving Arbitrary via (GenericArbitrary R1)

data R2 = R2 R1
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass NFData
  deriving Arbitrary via (GenericArbitrary R2)

-- To force the instance realy be generated
usage :: IO ()
usage = do
  r1 :: R1 <- generate arbitrary
  print r1 -- haha

{-# OPTIONS_GHC -fdefer-type-errors #-}

-- | Test that infinite types has no Arbitrary instance

module NoTypecheckTest where

import           Control.Exception
import           GHC.Generics                      (Generic)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic

-- | Recursive infinite type which can not have valid Arbitrary instance
data R = R R
  deriving (Eq, Show, Generic)

-- | Instance which must not compile, but we are using deferred type errors
instance Arbitrary R where
  arbitrary = genericArbitrary
  shrink = genericShrink

unit_mustFail :: IO ()
unit_mustFail = do
  try (generate (arbitrary :: Gen R) >>= print) >>= \case
    Left (_ :: SomeException) -> pure ()
    Right _ -> error "Error expected"

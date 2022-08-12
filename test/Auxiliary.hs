module Auxiliary where

import           Control.Exception
import           Test.QuickCheck

failGeneration :: forall a. (Arbitrary a, Show a) => IO ()
failGeneration = do
  try (generate (arbitrary :: Gen a) >>= print) >>= \case
    Left (_ :: SomeException) -> pure ()
    Right _ -> error "Error expected"

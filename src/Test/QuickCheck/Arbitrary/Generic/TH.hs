{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.QuickCheck.Arbitrary.Generic.TH where

import Language.Haskell.TH
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

deriveArbitrary :: Name -> Q [Dec]
deriveArbitrary name =
  [d|instance Arbitrary $(pure $ ConT name) where
       arbitrary = genericArbitrary
       shrink = genericShrink|]

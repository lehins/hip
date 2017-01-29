{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image.Interface.RepaSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Graphics.Image as I
import Graphics.Image.Interface.Repa

import Graphics.Image.InterfaceSpec ()

prop_toFromRepaS
  :: Image RSU Y Word8 -> Bool
prop_toFromRepaS img = img == fromRepaArrayS (toRepaArray img)

prop_toFromRepaP
  :: Image RSU Y Word8 -> Bool
prop_toFromRepaP img = img == fromRepaArrayS (toRepaArray img)


spec :: Spec
spec = do
  describe "Vector Representation Properties" $ do
    it "toFromRepaS" $ property $ prop_toFromRepaS
    it "toFromRepaP" $ property $ prop_toFromRepaP

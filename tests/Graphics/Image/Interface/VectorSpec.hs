{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image.Interface.VectorSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Graphics.Image.Interface as I

import Graphics.Image.InterfaceSpec ()

prop_fromToIx :: Positive Int -> (NonNegative Int, NonNegative Int) -> Bool
prop_fromToIx (Positive n) (NonNegative i, NonNegative j) =
  (i, j `mod` n) == toIx n (fromIx n (i, j `mod` n))

prop_toFromIx :: Positive Int -> NonNegative Int -> Bool
prop_toFromIx (Positive n) (NonNegative k) = k == fromIx n (toIx n k)

spec :: Spec
spec = do
  describe "Vector Index Properties" $ do
    it "fromToIx" $ property $ prop_fromToIx
    it "toFromIx" $ property $ prop_toFromIx

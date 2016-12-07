{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image.Interface.VectorSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Graphics.Image as IM
import qualified Graphics.Image.Interface.Vector as IV
import Graphics.Image.Types

import Graphics.Image.InterfaceSpec ()

prop_fromToIx :: Positive Int -> (NonNegative Int, NonNegative Int) -> Bool
prop_fromToIx (Positive n) (NonNegative i, NonNegative j) =
  (i, j `mod` n) == IV.toIx n (IV.fromIx n (i, j `mod` n))

prop_toFromIx :: Positive Int -> NonNegative Int -> Bool
prop_toFromIx (Positive n) (NonNegative k) = k == IV.fromIx n (IV.toIx n k)

prop_toFromVector
  :: Image VU Y Word8 -> Bool
prop_toFromVector img = img == IV.fromUnboxedVector (IM.dims img) (IV.toUnboxedVector img)

spec :: Spec
spec = do
  describe "Vector Representation Properties" $ do
    it "fromToIx" $ property $ prop_fromToIx
    it "toFromIx" $ property $ prop_toFromIx
    it "toFromVector" $ property $ prop_toFromVector

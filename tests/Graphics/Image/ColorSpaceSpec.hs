{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Image.ColorSpaceSpec (spec) where

import           Graphics.Image           as I
import           Graphics.Image.Interface as II
import           Test.Hspec
import           Test.QuickCheck


instance Arbitrary (Pixel X Bit) where
  arbitrary = elements [on, off]

-- | Generator for values in range @[0, 1]@
arbitraryDouble :: Gen Double
arbitraryDouble = toDouble <$> (arbitrary :: Gen Word64)


instance Arbitrary (Pixel Y Word8) where
  arbitrary = PixelY <$> arbitrary

instance Arbitrary (Pixel YA Word8) where
  arbitrary = PixelYA <$> arbitrary <*> arbitrary

instance Arbitrary (Pixel Y Double) where
  arbitrary = PixelY <$> arbitraryDouble

instance Arbitrary (Pixel YA Double) where
  arbitrary = PixelYA <$> arbitraryDouble <*> arbitraryDouble

instance Arbitrary (Pixel RGB Word8) where
  arbitrary = PixelRGB <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Pixel RGBA Word8) where
  arbitrary = PixelRGBA <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Pixel RGB Double) where
  arbitrary = PixelRGB <$> arbitraryDouble <*> arbitraryDouble <*> arbitraryDouble

instance Arbitrary (Pixel RGBA Double) where
  arbitrary =
    PixelRGBA <$> arbitraryDouble <*> arbitraryDouble <*> arbitraryDouble <*>
    arbitraryDouble


prop_ToFromComponents :: ColorSpace cs e => Pixel cs e -> Bool
prop_ToFromComponents px = px == fromComponents (toComponents px)


prop_ToFromRGB :: ToRGB cs e
               => (Pixel RGB Double -> Pixel cs e)
               -> Pixel RGB Double
               -> Bool
prop_ToFromRGB fromRGB pxRGB = eqTolPx 1 pxRGB8 pxRGB8' where
  pxRGB8 = toWord8 <$> pxRGB
  pxRGB8' = toWord8 <$> toPixelRGB (fromRGB pxRGB)


spec :: Spec
spec = do
  describe "ToFromComponents" $ do
    it "Y Components" $ property (prop_ToFromComponents :: Pixel Y Double -> Bool)
    it "YA Components" $ property (prop_ToFromComponents :: Pixel YA Double -> Bool)
    it "RGB Components" $ property (prop_ToFromComponents :: Pixel RGB Double -> Bool)
    it "RGBA Components" $ property (prop_ToFromComponents :: Pixel RGBA Double -> Bool)
  describe "YCbCr" $ do
    it "To From RGB" $ property (prop_ToFromRGB toPixelYCbCr)
  describe "HSI" $ do
    it "To From RGB" $ property (prop_ToFromRGB toPixelHSI)

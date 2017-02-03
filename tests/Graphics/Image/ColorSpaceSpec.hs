{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Image.ColorSpaceSpec (spec) where

import Test.Hspec
import Test.QuickCheck
  
import Graphics.Image as I
import Graphics.Image.Interface as II


instance Arbitrary (Pixel Binary Bit) where
  arbitrary = elements [on, off]

instance (ColorSpace Y e, Arbitrary e) => Arbitrary (Pixel Y e) where
  arbitrary = fromComponents <$> arbitrary

instance (ColorSpace YA e, Arbitrary e) => Arbitrary (Pixel YA e) where
  arbitrary = fromComponents <$> arbitrary

instance (ColorSpace RGB e, Arbitrary e) => Arbitrary (Pixel RGB e) where
  arbitrary = fromComponents <$> arbitrary

instance (ColorSpace RGBA e, Arbitrary e) => Arbitrary (Pixel RGBA e) where
  arbitrary = fromComponents <$> arbitrary


prop_ToFromComponents :: ColorSpace cs e =>
                         Pixel cs e -> Bool
prop_ToFromComponents px = px == fromComponents (toComponents px)


spec :: Spec
spec = describe "ColorSpace" $ do
  it "RGBComponents" $ property (prop_ToFromComponents :: Pixel RGB Double -> Bool)

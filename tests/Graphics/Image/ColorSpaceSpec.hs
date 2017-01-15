{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Image.ColorSpaceSpec (spec) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Test.Hspec
import Test.QuickCheck
  
import Graphics.Image as I
import Graphics.Image.Interface as II
import Graphics.Image.ColorSpace


instance Arbitrary RGB where
  arbitrary = do
    NonNegative c <- arbitrary
    return $ toEnum (c `mod` 3)

instance Arbitrary e => Arbitrary (Pixel RGB e) where
  arbitrary = PixelRGB <$> arbitrary <*> arbitrary <*> arbitrary


instance (MArray VU RGB e, Arbitrary e) => Arbitrary (Image VU RGB e) where
  arbitrary = do
    (Positive m, Positive n) <- arbitrary
    II.mapM (const arbitrary) $ I.makeImage (m, n) (const $ PixelGray (0 :: Double))
  

prop_ToFromComponents :: (ColorSpace cs e, Eq (Pixel cs e)) =>
                         Pixel cs e -> Bool
prop_ToFromComponents px = px == fromComponents (toComponents px)


spec :: Spec
spec = describe "ColorSpace" $ do
  it "RGBComponents" $ property (prop_ToFromComponents :: Pixel RGB Double -> Bool)

{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image.ProcessingSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Graphics.Image.Interface as I
import Graphics.Image.Types
import Graphics.Image.Processing

import Graphics.Image.InterfaceSpec (translateWrap, dummyImage10x20)

data Interpol
  = I1 Nearest
  | I2 Bilinear

instance Show Interpol where
  show (I1 i) = "I1 " ++ show i
  show (I2 i) = "I2 " ++ show i

instance Arbitrary Interpol where
  arbitrary = do
    ix <- arbitrary
    case ix `mod` (2 :: Int) of
      0 -> return $ I1 Nearest
      1 -> return $ I2 Bilinear
      _ -> error $ "Unknown interpolation: " ++ show ix


prop_sampleRows :: Image VU Y Double -> Bool
prop_sampleRows img = img == downsampleRows (upsampleRows img)

prop_sampleCols :: Image VU Y Double -> Bool
prop_sampleCols img = img == downsampleCols (upsampleCols img)

prop_sample :: Image VU Y Double -> Bool
prop_sample img = img == downsample (upsample img)

prop_translateWrap :: (Int, Int) -> Image VU RGB Double -> Bool
prop_translateWrap shift img = translateWrap shift img == translate Wrap shift img

prop_cropSuperimpose :: (Positive (Small Int), Positive (Small Int))
                     -> (Positive (Small Int), Positive (Small Int))
                     -> Image VU Y Double -> Bool
prop_cropSuperimpose (Positive (Small iA), Positive (Small jA)) (Positive (Small mA), Positive (Small nA)) img =
  img == superimpose (i0, j0) (crop (i0, j0) (m', n') img) img
  where
    (m, n) = I.dims img
    (i0, j0) = (iA `mod` m, jA `mod` n)
    (m', n') = (1 + mA `mod` (m - i0), 1 + nA `mod` (n - j0))
    
prop_concatRotate :: Image VU Y Word8 -> Bool
prop_concatRotate img =
  topToBottom (rotate90 img) (rotate270 img) ==
  rotate90 (leftToRight img $ rotate180 img)


prop_rotate90 :: Interpol -> Border (Pixel RGB Double) -> Image VU RGB Double -> Bool
prop_rotate90 (I1 i) border img = rotate90 img == rotate i border (pi/2) img
prop_rotate90 (I2 i) border img = rotate90 img == rotate i border (pi/2) img

prop_rotate180 :: Interpol -> Border (Pixel RGB Double) -> Image VU RGB Double -> Bool
prop_rotate180 (I1 i) border img = rotate180 img == rotate i border pi img
prop_rotate180 (I2 i) border img = rotate180 img == rotate i border pi img

prop_rotate270 :: Interpol -> Border (Pixel RGB Double) -> Image VU RGB Double -> Bool
prop_rotate270 (I1 i) border img = rotate270 img == rotate i border (3*pi/2) img
prop_rotate270 (I2 i) border img = rotate270 img == rotate i border (3*pi/2) img

prop_rotate360 :: Interpol -> Border (Pixel RGB Double) -> Image VU RGB Double -> Bool
prop_rotate360 (I1 i) border img = (rotate270 . rotate90) img == rotate i border (2*pi) img
prop_rotate360 (I2 i) border img = (rotate270 . rotate90) img == rotate i border (2*pi) img


spec :: Spec
spec = do
  describe "Processing Properties" $
    do it "sampleRows" $ property prop_sampleRows
       it "sampleCols" $ property prop_sampleCols
       it "sample" $ property prop_sample
       it "translateWrap" $ property prop_translateWrap
       it "cropSuperimpose" $ property prop_cropSuperimpose
       it "concatRotate" $ property prop_concatRotate
       it "rotate90" $ property prop_rotate90
       it "rotate180" $ property prop_rotate180
       it "rotate270" $ property prop_rotate270
       it "rotate360" $ property prop_rotate360
  describe "Processing Errors" $
    do it "crop start index outside" $
         do shouldThrow (return $! crop (-1, -1) (1, 1) dummyImage10x20) anyException
            shouldThrow (return $! crop (10, 20) (1, 1) dummyImage10x20) anyException
            shouldThrow (return $! crop (15, 10) (1, 1) dummyImage10x20) anyException
            shouldThrow (return $! crop (5, 21) (1, 1) dummyImage10x20) anyException
       it "crop result image outside" $
         do shouldThrow (return $! crop (6, 6) (5, 15) dummyImage10x20) anyException
            shouldThrow (return $! crop (5, 15) (5, 15) dummyImage10x20) anyException
       it "crop negative dimensions" $
         do shouldThrow (return $! crop (1, 1) (-5, 15) dummyImage10x20) anyException
            shouldThrow (return $! crop (1, 1) (5, -15) dummyImage10x20) anyException
       it "upsample non-positive" $
         do shouldThrow (return $! upsampleF (0, 1) dummyImage10x20) anyException
            shouldThrow (return $! upsampleF (1, 0) dummyImage10x20) anyException
            shouldThrow (return $! upsampleF (-1, -1) dummyImage10x20) anyException
       it "downsample non-positive" $
         do shouldThrow (return $! downsampleF (0, 1) dummyImage10x20) anyException
            shouldThrow (return $! downsampleF (1, 0) dummyImage10x20) anyException
            shouldThrow (return $! downsampleF (-1, -1) dummyImage10x20) anyException
       it "concat dimension mismatch" $
         do shouldThrow
              (return $! leftToRight dummyImage10x20 $ I.transpose dummyImage10x20)
              anyException
            shouldThrow
              (return $! topToBottom dummyImage10x20 $ I.transpose dummyImage10x20)
              anyException

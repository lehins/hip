{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image.ProcessingSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Graphics.Image as I

import Graphics.Image.InterfaceSpec (translateWrap, dummyImage10x20)


data Interpol
  = I1 Nearest
  | I2 Bilinear
  | I3 Bicubic

instance Show Interpol where
  show (I1 i) = "I1 " ++ show i
  show (I2 i) = "I2 " ++ show i
  show (I3 i) = "I3 " ++ show i

instance Arbitrary Interpol where
  arbitrary = do
    ix <- arbitrary
    case ix `mod` (3 :: Int) of
      0 -> return $ I1 Nearest
      1 -> return $ I2 Bilinear
      2 -> do
        Negative a <- arbitrary
        return . I3 $ Bicubic a
      _ -> error $ "Unknown interpolation: " ++ show ix


prop_sampleRows :: Image VU Y Double -> Bool
prop_sampleRows img = img == downsampleRows (upsampleRows img)

prop_sampleCols :: Image VU Y Double -> Bool
prop_sampleCols img = img == downsampleCols (upsampleCols img)

prop_upsampleNegative :: NonNegative Int -> NonNegative Int
                      -> NonNegative Int -> NonNegative Int
                      -> Image VU Y Double -> Property
prop_upsampleNegative (NonNegative p1) (NonNegative p2) (NonNegative p3) (NonNegative p4) img =
  p1 /= 0 && p2 /= 0 && p3 /= 0 && p4 /= 0 ==>
  expectFailure (upsample (const (-p1, -p2)) (const (-p3, -p4)) img `seq` True)

prop_upsampleId :: Image VU Y Double -> Bool
prop_upsampleId img = img == upsample (const (0,0)) (const (0,0)) img

prop_downsampleId :: Image VU Y Double -> Bool
prop_downsampleId img = img == downsample (const False) (const False) img

prop_sampleEven :: Image VU Y Double -> Bool
prop_sampleEven img = img == downsample even even (upsample (const (1,0)) (const (1,0)) img)

prop_sampleOdd :: Image VU Y Double -> Bool
prop_sampleOdd img = img == downsample odd odd (upsample (const (0,1)) (const (0,1)) img)

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
prop_rotate90 (I3 i) border img = rotate90 img == rotate i border (pi/2) img

prop_rotate180 :: Interpol -> Border (Pixel RGB Double) -> Image VU RGB Double -> Bool
prop_rotate180 (I1 i) border img = rotate180 img == rotate i border pi img
prop_rotate180 (I2 i) border img = rotate180 img == rotate i border pi img
prop_rotate180 (I3 i) border img = rotate180 img == rotate i border pi img

prop_rotate270 :: Interpol -> Border (Pixel RGB Double) -> Image VU RGB Double -> Bool
prop_rotate270 (I1 i) border img = rotate270 img == rotate i border (3*pi/2) img
prop_rotate270 (I2 i) border img = rotate270 img == rotate i border (3*pi/2) img
prop_rotate270 (I3 i) border img = rotate270 img == rotate i border (3*pi/2) img

prop_rotate360 :: Interpol -> Border (Pixel RGB Double) -> Image VU RGB Double -> Bool
prop_rotate360 (I1 i) border img = (rotate270 . rotate90) img == rotate i border (2*pi) img
prop_rotate360 (I2 i) border img = (rotate270 . rotate90) img == rotate i border (2*pi) img
prop_rotate360 (I3 i) border img = (rotate270 . rotate90) img == rotate i border (2*pi) img


struct :: Image VS X Bit
struct = fromLists [[0,1,0],[1,1,0],[0,1,0]]


spec :: Spec
spec = do
  describe "Processing Properties" $
    do it "[up/down]sampleRows" $ property prop_sampleRows
       it "[up/down]sampleCols" $ property prop_sampleCols
       it "[up/down]sampleId" $ property $ conjoin [prop_upsampleId, prop_downsampleId]
       it "[up/down]sampleEven" $ property prop_sampleEven
       it "[up/down]sampleOdd" $ property prop_sampleOdd
       it "translateWrap" $ property prop_translateWrap
       it "cropSuperimpose" $ property prop_cropSuperimpose
       it "concatRotate" $ property prop_concatRotate
       it "rotate90" $ property prop_rotate90
       it "rotate180" $ property prop_rotate180
       it "rotate270" $ property prop_rotate270
       it "rotate360" $ property prop_rotate360
  describe "Processing Unit Tests" $
    do it "upsampleRows" $ upsampleRows struct `shouldBe`
         I.fromLists [[0,1,0],[0,0,0],[1,1,0],[0,0,0],[0,1,0],[0,0,0]]
       it "upsampleCols" $ upsampleCols struct `shouldBe`
         I.fromLists [[0,0,1,0,0,0],[1,0,1,0,0,0],[0,0,1,0,0,0]]
       it "downsampleRows" $ downsampleRows struct `shouldBe` I.fromLists [[0,1,0],[0,1,0]]
       it "downsampleCols" $ downsampleCols struct `shouldBe` I.fromLists [[0,0],[1,0],[0,0]]
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
       it "upsampleNegative" $ property prop_upsampleNegative
       it "downsample all" $
         do shouldThrow (return $! downsample (const True) even dummyImage10x20) anyException
            shouldThrow (return $! downsample even (const True) dummyImage10x20) anyException
            shouldThrow (return $! downsample (const True) (const True) dummyImage10x20)
                        anyException
       it "concat dimension mismatch" $
         do shouldThrow
              (return $! leftToRight dummyImage10x20 $ I.transpose dummyImage10x20)
              anyException
            shouldThrow
              (return $! topToBottom dummyImage10x20 $ I.transpose dummyImage10x20)
              anyException

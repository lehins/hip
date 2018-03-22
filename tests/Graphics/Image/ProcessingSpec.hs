{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Image.ProcessingSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Graphics.Image as I

--import Graphics.Image.InterfaceSpec (translateWrap, dummyImage10x20)

instance Arbitrary px => Arbitrary (Border px) where
  arbitrary = oneof [Fill <$> arbitrary, pure Wrap, pure Edge, pure Reflect, pure Continue]

instance (ColorSpace cs e, Arbitrary (Components cs e)) => Arbitrary (Pixel cs e) where
  arbitrary = fromComponents <$> arbitrary

instance (ColorSpace cs e, Arbitrary (Pixel cs e), Arbitrary e) =>
         Arbitrary (Image cs e) where
  arbitrary = do
    f <- arbitrary
    sz <- arbitrary
    return $ makeImage (toIx2 sz) (\ix -> f (fromIx2 ix))

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


prop_sampleRows :: Image Y Double -> Bool
prop_sampleRows img = img == downsampleRows (upsampleRows img)

prop_sampleCols :: Image Y Double -> Bool
prop_sampleCols img = img == downsampleCols (upsampleCols img)

prop_upsampleNegative :: Positive Int -> Positive Int
                      -> Positive Int -> Positive Int
                      -> Image Y Double -> Property
prop_upsampleNegative (Positive p1) (Positive p2) (Positive p3) (Positive p4) img =
  img === upsample 0 (const (-p1, -p2)) (const (-p3, -p4)) img

prop_upsampleId :: Image Y Double -> Bool
prop_upsampleId img = img == upsample 0 (const (0,0)) (const (0,0)) img

prop_downsampleId :: Image Y Double -> Bool
prop_downsampleId img = img == downsample (const False) (const False) img

prop_sampleEven :: Image Y Double -> Bool
prop_sampleEven img = img == downsample even even (upsample 0 (const (1,0)) (const (1,0)) img)

prop_sampleOdd :: Image Y Double -> Bool
prop_sampleOdd img = img == downsample odd odd (upsample 0 (const (0,1)) (const (0,1)) img)

-- prop_translateWrap :: (Int, Int) -> Image RGB Double -> Bool
-- prop_translateWrap shift img = translateWrap shift img == translate Wrap shift img

prop_cropSuperimpose :: (Positive (Small Int), Positive (Small Int))
                     -> (Positive (Small Int), Positive (Small Int))
                     -> Image Y Double -> Property
prop_cropSuperimpose (Positive (Small iA), Positive (Small jA)) (Positive (Small mA), Positive (Small nA)) img =
  m /= 0 && n /= 0 ==> img === superimpose ix0 (crop (const (ix0, sz)) img) img
  where
    (m :. n) = I.dims img
    ix0@(i0 :. j0) = (iA `mod` m) :. (jA `mod` n)
    sz = (1 + mA `mod` (m - i0)) :. (1 + nA `mod` (n - j0))

prop_concatRotate :: Image Y Word8 -> Bool
prop_concatRotate img =
  topToBottom (rotate90 img) (rotate270 img) ==
  rotate90 (leftToRight img $ rotate180 img)


prop_rotate90 :: Interpol -> Border (Pixel RGB Double) -> Image RGB Double -> Bool
prop_rotate90 (I1 i) border img = rotate90 img == rotate i border (pi/2) img
prop_rotate90 (I2 i) border img = rotate90 img == rotate i border (pi/2) img

prop_rotate180 :: Interpol -> Border (Pixel RGB Double) -> Image RGB Double -> Bool
prop_rotate180 (I1 i) border img = rotate180 img == rotate i border pi img
prop_rotate180 (I2 i) border img = rotate180 img == rotate i border pi img

prop_rotate270 :: Interpol -> Border (Pixel RGB Double) -> Image RGB Double -> Bool
prop_rotate270 (I1 i) border img = rotate270 img == rotate i border (3*pi/2) img
prop_rotate270 (I2 i) border img = rotate270 img == rotate i border (3*pi/2) img

prop_rotate360 :: Interpol -> Border (Pixel RGB Double) -> Image RGB Double -> Bool
prop_rotate360 (I1 i) border img = (rotate270 . rotate90) img == rotate i border (2*pi) img
prop_rotate360 (I2 i) border img = (rotate270 . rotate90) img == rotate i border (2*pi) img


struct :: Image X Bit
struct = fromLists [[0,1,0],[1,1,0],[0,1,0]]


spec :: Spec
spec = do
  describe "Processing Properties" $ do
    it "[up/down]sampleRows" $ property prop_sampleRows
    it "[up/down]sampleCols" $ property prop_sampleCols
    it "upsampleId" $ property prop_upsampleId
    it "downsampleId" $ property prop_downsampleId
    it "[up/down]sampleEven" $ property prop_sampleEven
    it "[up/down]sampleOdd" $ property prop_sampleOdd
    -- it "translateWrap" $ property prop_translateWrap
    it "cropSuperimpose" $ property prop_cropSuperimpose
    it "concatRotate" $ property prop_concatRotate
    it "rotate90" $ property prop_rotate90
    it "rotate180" $ property prop_rotate180
    it "rotate270" $ property prop_rotate270
    it "rotate360" $ property prop_rotate360
  describe "Processing Unit Tests" $ do
    it "upsampleRows" $
      upsampleRows struct `shouldBe`
      I.fromLists [[0, 1, 0], [0, 0, 0], [1, 1, 0], [0, 0, 0], [0, 1, 0], [0, 0, 0]]
    it "upsampleCols" $
      upsampleCols struct `shouldBe`
      I.fromLists [[0, 0, 1, 0, 0, 0], [1, 0, 1, 0, 0, 0], [0, 0, 1, 0, 0, 0]]
    it "downsampleRows" $ downsampleRows struct `shouldBe` I.fromLists [[0, 1, 0], [0, 1, 0]]
    it "downsampleCols" $ downsampleCols struct `shouldBe` I.fromLists [[0, 0], [1, 0], [0, 0]]
  -- describe "Processing Errors" $ do
    -- it "crop start index outside" $ do
    --   shouldThrow (return $! crop (-1, -1) (1, 1) dummyImage10x20) anyException
    --   shouldThrow (return $! crop (10, 20) (1, 1) dummyImage10x20) anyException
    --   shouldThrow (return $! crop (15, 10) (1, 1) dummyImage10x20) anyException
    --   shouldThrow (return $! crop (5, 21) (1, 1) dummyImage10x20) anyException
    -- it "crop result image outside" $ do
    --   shouldThrow (return $! crop (6, 6) (5, 15) dummyImage10x20) anyException
    --   shouldThrow (return $! crop (5, 15) (5, 15) dummyImage10x20) anyException
    -- it "crop negative dimensions" $ do
    --   shouldThrow (return $! crop (1, 1) (-5, 15) dummyImage10x20) anyException
    --   shouldThrow (return $! crop (1, 1) (5, -15) dummyImage10x20) anyException
    it "upsampleNegative" $ property prop_upsampleNegative
    -- it "downsample all" $ do
    --   shouldThrow (return $! downsample (const True) even dummyImage10x20) anyException
    --   shouldThrow (return $! downsample even (const True) dummyImage10x20) anyException
    --   shouldThrow (return $! downsample (const True) (const True) dummyImage10x20) anyException
    -- it "concat dimension mismatch" $ do
    --   shouldThrow (return $! leftToRight dummyImage10x20 $ I.transpose dummyImage10x20) anyException
    --   shouldThrow (return $! topToBottom dummyImage10x20 $ I.transpose dummyImage10x20) anyException

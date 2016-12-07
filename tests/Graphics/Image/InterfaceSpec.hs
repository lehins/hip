{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Image.InterfaceSpec
  ( spec
  , translateWrap
  , dummyImage10x20
  , Identical (..)
  ) where

#if MIN_VERSION_base(4,8,0)
import Data.Typeable (Typeable, typeOf)
#else
import Control.Applicative
#endif
import Test.Hspec
import Test.QuickCheck

import qualified Graphics.Image as IM
import qualified Graphics.Image.Interface as I
import Graphics.Image.Types
import Graphics.Image.Processing


data Identical arr1 arr2 cs e =
  Identical (Image arr1 cs e)
       (Image arr2 cs e)
  deriving (Show)

-- | Generator for values in range @[0, 1]@
arbitraryDouble :: Gen Double
arbitraryDouble = do
  Positive v <- arbitrary
  let v' = v - fromInteger (floor v)
  if v' == 0
    then choose (0, 1)
    else return v'

instance Arbitrary (Pixel Y Word8) where
  arbitrary = PixelY <$> arbitrary

instance Arbitrary (Pixel Y Double) where
  arbitrary = PixelY <$> arbitraryDouble

instance Arbitrary (Pixel RGB Word8) where
  arbitrary = PixelRGB <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Pixel RGB Double) where
  arbitrary = PixelRGB <$> arbitraryDouble <*> arbitraryDouble <*> arbitraryDouble

instance (SequentialArray arr cs e, Arbitrary (Pixel cs e)) => Arbitrary (Image arr cs e) where
  arbitrary = do
    (Positive (Small m), Positive (Small n)) <- arbitrary
    I.makeImageM (m, n) (const arbitrary)


instance (Array arr1 cs e, Array arr2 cs e, Arbitrary (Pixel cs e)) =>
         Arbitrary (Identical arr1 arr2 cs e) where
  arbitrary = do
    (Positive (Small m), Positive (Small n)) <- arbitrary
    getPx <- arbitrary
    return $ Identical (I.makeImage (m, n) getPx) (I.makeImage (m, n) getPx)


instance Arbitrary px => Arbitrary (Border px) where
  arbitrary = do
    methodIx <- arbitrary
    case methodIx `mod` 5 :: Int of
      0 -> Fill <$> arbitrary
      1 -> return Wrap
      2 -> return Edge
      3 -> return Reflect
      4 -> return Continue
      _ -> error "Unknown method"


#if MIN_VERSION_base(4,8,0)
instance (Typeable a, Typeable b) => Show (a -> b) where
  show _ = show $ typeOf (undefined :: a -> b)
#else
instance Show (a -> b) where
  show _ = "Function"
#endif


instance CoArbitrary e => CoArbitrary (Pixel Y e) where
  coarbitrary (PixelY y) = coarbitrary y


dummyImage10x20 :: Image VU Y Word8
dummyImage10x20 = I.makeImage (10, 20) (fromIntegral . (uncurry (+)))


prop_borderIndex
  :: Border (Pixel RGB Double)
  -> Image VU RGB Double
  -> (Positive Int, Positive Int)
  -> Bool
prop_borderIndex border img (Positive i, Positive j) =
  I.borderIndex border img (iOut, jOut) == I.index bigImg (iBig, jBig)
  where
    bigImg = foldr1 topToBottom $ map (foldr1 leftToRight) imgs
    (m, n) = I.dims img
    (iBig, jBig) = (i `mod` (3 * m), j `mod` (3 * n))
    (iOut, jOut) = (iBig - m, jBig - n)
    imgs =
      case border of
        Fill px ->
          let filled = I.map (const px) img
          in [ [filled, filled, filled]
             , [filled, img, filled]
             , [filled, filled, filled]
             ]
        Wrap -> [[img, img, img], [img, img, img], [img, img, img]]
        Edge ->
          [ [ I.traverse img id (\getPx _ -> getPx (0, 0))
            , I.traverse img id (\getPx (_, j') -> getPx (0, j'))
            , I.traverse img id (\getPx _ -> getPx (0, n - 1))
            ]
          , [ I.traverse img id (\getPx (i', _) -> getPx (i', 0))
            , img
            , I.traverse img id (\getPx (i', _) -> getPx (i', n - 1))
            ]
          , [ I.traverse img id (\getPx _ -> getPx (m - 1, 0))
            , I.traverse img id (\getPx (_, j') -> getPx (m - 1, j'))
            , I.traverse img id (\getPx _ -> getPx (m - 1, n - 1))
            ]
          ]
        Reflect ->
          let h = flipH img
              v = flipV img
              c = flipH v
          in [[c, v, c], [h, img, h], [c, v, c]]
        Continue ->
          let h = flipH img
              v = flipV img
              c = flipH v
          in
          [ [ translateWrap (1, 1) c
            , translateWrap (1, 0) v
            , translateWrap (1, -1) c
            ]
          , [translateWrap (0, 1) h, img, translateWrap (0, -1) h]
          , [ translateWrap (-1, 1) c
            , translateWrap (-1, 0) v
            , translateWrap (-1, -1) c
            ]
          ]


translateWrap :: Array arr cs e => (Int, Int) -> Image arr cs e -> Image arr cs e
translateWrap (dm, dn) img = I.traverse img id newPx
  where
    (m, n) = I.dims img
    newPx getPx (i, j) = getPx ((i - dm) `mod` m, (j - dn) `mod` n)


prop_toFormLists :: Image VU Y Word8 -> Bool
prop_toFormLists img = img == I.fromLists (IM.toLists img)


prop_sameDims :: Array arr Y Word8 => arr -> Identical VU arr Y Word8 -> Bool
prop_sameDims _ (Identical img1 img2) = IM.dims img1 == IM.dims img2

prop_sameImage
  :: (Exchangable arr RS, Array arr Y Word8)
  => arr -> Identical VU arr Y Word8 -> Bool
prop_sameImage _ (Identical img1 img2) = I.exchange RS img1 == I.exchange RS img2

prop_sameMap
  :: (Exchangable arr RS, Array arr Y Word8)
  => arr -> (Pixel Y Word8 -> Pixel Y Word8) -> Identical VU arr Y Word8 -> Bool
prop_sameMap _ f (Identical img1 img2) =
  I.exchange RS (I.map f img1) == I.exchange RS (I.map f img2)

prop_sameImap
  :: (Exchangable arr RP, Array arr Y Word8)
  => arr -> ((Int, Int) -> Pixel Y Word8 -> Pixel Y Word8) -> Identical VU arr Y Word8 -> Bool
prop_sameImap _ f (Identical img1 img2) =
  I.exchange RP (I.imap f img1) == I.exchange RP (I.imap f img2)


prop_sameZipWith
  :: (Exchangable arr RP, Array arr Y Word8)
  => arr
  -> (Pixel Y Word8 -> Pixel Y Word8)
  -> (Pixel Y Word8 -> Pixel Y Word8 -> Pixel Y Word8)
  -> Identical VU arr Y Word8
  -> Bool
prop_sameZipWith _ g f (Identical img1 img2) =
  I.exchange RP (I.zipWith f img1 img1') ==
  I.exchange RP (I.zipWith f img2 img2')
  where
    img1' = I.map g img1
    img2' = I.map g img2

prop_sameIZipWith
  :: (Exchangable arr RP, Array arr Y Word8)
  => arr
  -> (Pixel Y Word8 -> Pixel Y Word8)
  -> ((Int, Int) -> Pixel Y Word8 -> Pixel Y Word8 -> Pixel Y Word8)
  -> Identical VU arr Y Word8
  -> Bool
prop_sameIZipWith _ g f (Identical img1 img2) =
  I.exchange RP (I.izipWith f img1 img1') ==
  I.exchange RP (I.izipWith f img2 img2')
  where
    img1' = I.map g img1
    img2' = I.map g img2

prop_sameTraverse
  :: (Exchangable arr RS, Array arr Y Word8)
  => arr
  -> ((Int, Int) -> (Positive (Small Int), Positive (Small Int)))
  -> ((Int, Int) -> Pixel Y Word8 -> Pixel Y Word8)
  -> Identical VU arr Y Word8
  -> Bool
prop_sameTraverse _ g f (Identical img1 img2) =
  I.exchange RS (I.traverse img1 (g' . g) f') ==
  I.exchange RS (I.traverse img2 (g' . g) f')
  where
    g' (Positive (Small i), Positive (Small j)) = (i, j)
    f' getPx ix@(i, j) = f ix (getPx (i `mod` m, j `mod` n))
    (m, n) = I.dims img1


spec :: Spec
spec = do
  describe "Interface Properties" $ do
    it "borderIndex" $ property prop_borderIndex
    it "toFormLists" $ property prop_toFormLists
  describe "Representation Properties" $ do
    it "sameDims RD" $ property $ prop_sameDims RD
    it "sameDims RS" $ property $ prop_sameDims RS
    it "sameDims RP" $ property $ prop_sameDims RP
    it "sameImage RD" $ property $ prop_sameImage RD
    it "sameImage RS" $ property $ prop_sameImage RS
    it "sameImage RP" $ property $ prop_sameImage RP
    it "sameMap RD" $ property $ prop_sameMap RD
    it "sameMap RS" $ property $ prop_sameMap RS
    it "sameMap RP" $ property $ prop_sameMap RP
    it "sameImap RD" $ property $ prop_sameImap RD
    it "sameImap RS" $ property $ prop_sameImap RS
    it "sameImap RP" $ property $ prop_sameImap RP
    it "sameZipWith RD" $ property $ prop_sameZipWith RD
    it "sameZipWith RS" $ property $ prop_sameZipWith RS
    it "sameZipWith RP" $ property $ prop_sameZipWith RP
    it "sameIZipWith RD" $ property $ prop_sameIZipWith RD
    it "sameIZipWith RS" $ property $ prop_sameIZipWith RS
    it "sameIZipWith RP" $ property $ prop_sameIZipWith RP
    it "sameTraverse RD" $ property $ prop_sameTraverse RD
    it "sameTraverse RS" $ property $ prop_sameTraverse RS
    it "sameTraverse RP" $ property $ prop_sameTraverse RP

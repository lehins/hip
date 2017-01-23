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

import Prelude as P
#if MIN_VERSION_base(4,8,0)
import Data.Typeable (Typeable, typeOf)
#else
import Control.Applicative
#endif
import Test.Hspec
import Test.QuickCheck

import Graphics.Image as I
import Graphics.Image.Interface as I


data Identical arr1 arr2 cs e =
  Identical (Image arr1 cs e)
       (Image arr2 cs e)
  deriving (Show)

-- | Generator for values in range @[0, 1]@
arbitraryDouble :: Gen Double
arbitraryDouble = toDouble <$> (arbitrary :: Gen Word64)


instance Arbitrary (Pixel Y Word8) where
  arbitrary = PixelY <$> arbitrary

instance Arbitrary (Pixel Y Double) where
  arbitrary = PixelY <$> arbitraryDouble

instance Arbitrary (Pixel RGB Word8) where
  arbitrary = PixelRGB <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Pixel RGB Double) where
  arbitrary = PixelRGB <$> arbitraryDouble <*> arbitraryDouble <*> arbitraryDouble

instance (Array arr cs e, MArray arr cs e, Arbitrary (Pixel cs e)) =>
         Arbitrary (Image arr cs e) where
  arbitrary = do
    (Positive (Small m), Positive (Small n)) <- arbitrary
    I.makeImageM (m, n) (const arbitrary)

  shrink img | dims img == (1,1) = []
             | rows img == 1 = [downsampleCols img]
             | cols img == 1 = [downsampleRows img]
             | otherwise = [downsample even even img, downsample odd odd img]

instance (Array arr1 cs e, Array arr2 cs e, Arbitrary (Pixel cs e)) =>
         Arbitrary (Identical arr1 arr2 cs e) where
  arbitrary = do
    (Positive (Small m), Positive (Small n)) <- arbitrary
    getPx <- arbitrary
    if (m, n) == (1, 1)
      then do
        img1 <- elements [I.makeImage (m, n) getPx, I.scalar (getPx (0, 0))]
        img2 <- elements [I.makeImage (m, n) getPx, I.scalar (getPx (0, 0))]
        return $ Identical img1 img2
      else return $
           Identical (I.makeImage (m, n) getPx) (I.makeImage (m, n) getPx)


instance Arbitrary px => Arbitrary (Border px) where
  arbitrary =
    oneof
      [ Fill <$> arbitrary
      , return Wrap
      , return Edge
      , return Reflect
      , return Continue
      ]
      

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
    bigImg = foldr1 topToBottom $ P.map (foldr1 leftToRight) imgs
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


prop_sameDims :: Array arr Y Word8 => arr -> Identical VU arr Y Word8 -> Bool
prop_sameDims _ (Identical img1 img2) = I.dims img1 == I.dims img2

prop_sameImage
  :: (Exchangable arr VU, Array arr RGB Word8)
  => arr -> Identical VU arr RGB Word8 -> Bool
prop_sameImage _ (Identical img1 img2) = img1 == I.exchange VU img2

prop_sameMap
  :: (Exchangable arr RSU, Array arr Y Word8)
  => arr -> (Pixel Y Word8 -> Pixel Y Word8) -> Identical VU arr Y Word8 -> Bool
prop_sameMap _ f (Identical img1 img2) =
  I.exchange RSU (I.map f img1) == I.exchange RSU (I.map f img2)

prop_sameImap
  :: (Exchangable arr RPU, Array arr Y Word8)
  => arr -> ((Int, Int) -> Pixel Y Word8 -> Pixel Y Word8) -> Identical VU arr Y Word8 -> Bool
prop_sameImap _ f (Identical img1 img2) =
  I.exchange RPU (I.imap f img1) == I.exchange RPU (I.imap f img2)


prop_sameZipWith
  :: (Exchangable arr RPU, Array arr Y Word8)
  => arr
  -> (Pixel Y Word8 -> Pixel Y Word8)
  -> (Pixel Y Word8 -> Pixel Y Word8 -> Pixel Y Word8)
  -> Identical VU arr Y Word8
  -> Bool
prop_sameZipWith _ g f (Identical img1 img2) =
  I.exchange RPU (I.zipWith f img1 img1') ==
  I.exchange RPU (I.zipWith f img2 img2')
  where
    img1' = I.map g img1
    img2' = I.map g img2

prop_sameIZipWith
  :: (Exchangable arr RPU, Array arr Y Word8)
  => arr
  -> (Pixel Y Word8 -> Pixel Y Word8)
  -> ((Int, Int) -> Pixel Y Word8 -> Pixel Y Word8 -> Pixel Y Word8)
  -> Identical VU arr Y Word8
  -> Bool
prop_sameIZipWith _ g f (Identical img1 img2) =
  I.exchange RPU (I.izipWith f img1 img1') ==
  I.exchange RPU (I.izipWith f img2 img2')
  where
    img1' = I.map g img1
    img2' = I.map g img2

prop_sameTraverse
  :: (Exchangable arr RSU, Array arr Y Word8)
  => arr
  -> ((Int, Int) -> (Positive (Small Int), Positive (Small Int)))
  -> ((Int, Int) -> Pixel Y Word8 -> Pixel Y Word8)
  -> Identical VU arr Y Word8
  -> Bool
prop_sameTraverse _ g f (Identical img1 img2) =
  I.exchange RSU (I.traverse img1 (g' . g) f') ==
  I.exchange RSU (I.traverse img2 (g' . g) f')
  where
    g' (Positive (Small i), Positive (Small j)) = (i, j)
    f' getPx ix@(i, j) = f ix (getPx (i `mod` m, j `mod` n))
    (m, n) = I.dims img1


prop_sameTraverse2
  :: (Exchangable arr RSU, Array arr Y Word8)
  => arr
  -> ((Int, Int) -> (Int, Int) -> (Positive (Small Int), Positive (Small Int)))
  -> ((Int, Int) -> Pixel Y Word8 -> Pixel Y Word8 -> Pixel Y Word8)
  -> Identical VU arr Y Word8
  -> Identical VU arr Y Word8
  -> Bool
prop_sameTraverse2 _ g f (Identical img1a img2a) (Identical img1b img2b) =
  I.exchange RSU (I.traverse2 img1a img1b g' f') ==
  I.exchange RSU (I.traverse2 img2a img2b g' f')
  where
    g' dimsA dimsB =
      case g dimsA dimsB of
        (Positive (Small i), Positive (Small j)) -> (i, j)
    f' getPx1 getPx2 ix@(i, j) =
      f ix (getPx1 (i `mod` ma, j `mod` na)) (getPx2 (i `mod` mb, j `mod` nb))
    (ma, na) = I.dims img1a
    (mb, nb) = I.dims img1b


prop_sameTranspose
  :: (Exchangable arr RSU, Array arr Y Word8)
  => arr
  -> Identical VU arr Y Word8
  -> Bool
prop_sameTranspose _ (Identical img1 img2) =
  I.exchange RSU (I.transpose img1) == I.exchange RSU (I.transpose img2)


prop_sameBackpermute
  :: (Exchangable arr RPU, Array arr Y Word8)
  => arr
  -> (Positive (Small Int), Positive (Small Int))
  -> ((Int, Int) -> (Int, Int))
  -> Identical VU arr Y Word8
  -> Bool
prop_sameBackpermute _ (Positive (Small m), Positive (Small n)) f (Identical img1 img2) =
  I.exchange RPU (I.backpermute (m, n) (f' . f) img1) ==
  I.exchange RPU (I.backpermute (m, n) (f' . f) img2)
  where
    (m', n') = I.dims img1
    f' (i, j) = (i `mod` m', j `mod` n')


prop_toFormLists :: (Array arr Y Word8, MArray arr Y Word8) => arr -> Image arr Y Word8 -> Bool
prop_toFormLists _ img = img == I.fromLists (I.toLists img)


spec :: Spec
spec = do
  describe "Interface Properties" $ do
    it "borderIndex" $ property prop_borderIndex
    it "toFormLists" $ property $ prop_toFormLists VU
  describe "Representation Properties" $ do
    it "sameDims VS" $ property $ prop_sameDims VS
    it "sameDims RSU" $ property $ prop_sameDims RSU
    it "sameDims RPU" $ property $ prop_sameDims RPU
    it "sameImage VS" $ property $ prop_sameImage VS
    it "sameImage RSU" $ property $ prop_sameImage RSU
    it "sameImage RPU" $ property $ prop_sameImage RPU
    --it "sameMap VS" $ property $ prop_sameMap VS
    it "sameMap RSU" $ property $ prop_sameMap RSU
    it "sameMap RPU" $ property $ prop_sameMap RPU
    --it "sameImap VS" $ property $ prop_sameImap VS
    it "sameImap RSU" $ property $ prop_sameImap RSU
    it "sameImap RPU" $ property $ prop_sameImap RPU
    --it "sameZipWith VS" $ property $ prop_sameZipWith VS
    it "sameZipWith RSU" $ property $ prop_sameZipWith RSU
    it "sameZipWith RPU" $ property $ prop_sameZipWith RPU
    --it "sameIZipWith VS" $ property $ prop_sameIZipWith VS
    it "sameIZipWith RSU" $ property $ prop_sameIZipWith RSU
    it "sameIZipWith RPU" $ property $ prop_sameIZipWith RPU
    --it "sameTraverse VS" $ property $ prop_sameTraverse VS
    it "sameTraverse RSU" $ property $ prop_sameTraverse RSU
    it "sameTraverse RPU" $ property $ prop_sameTraverse RPU
    --it "sameTraverse2 VS" $ property $ prop_sameTraverse2 VS
    it "sameTraverse2 RSU" $ property $ prop_sameTraverse2 RSU
    it "sameTraverse2 RPU" $ property $ prop_sameTraverse2 RPU
    --it "sameTranspose VS" $ property $ prop_sameTranspose VS
    it "sameTranspose RSU" $ property $ prop_sameTranspose RSU
    it "sameTranspose RPU" $ property $ prop_sameTranspose RPU
    --it "sameBackpermute VS" $ property $ prop_sameBackpermute VS
    it "sameBackpermute RSU" $ property $ prop_sameBackpermute RSU
    it "sameBackpermute RPU" $ property $ prop_sameBackpermute RPU

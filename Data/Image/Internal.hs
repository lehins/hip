{-# LANGUAGE ViewPatterns, TypeFamilies, FlexibleContexts #-}

module Data.Image.Internal (
  Image,
  Processable(..), Pixel(..), PixelOp
  ) where

import Data.Image.Base
import Data.Array.Repa
import Prelude hiding (map, zipWith)
import qualified Data.Vector.Unboxed as V

      
data Image px = VectorImage (Array U DIM2 px)
              | DelayedImage (Array D DIM2 px)
              | PureImage (Array D DIM0 px)


isSmall arr = w < 50 && h < 50 where (Z :. w  :. h) = extent arr

getInner (VectorImage arr) = delay arr
getInner (DelayedImage arr) = arr

instance Processable Image where
  width (VectorImage (extent -> (Z :. w :. _))) = w
  width (DelayedImage (extent -> (Z :. w :. _))) = w

  height (VectorImage (extent -> (Z :. _ :. h))) = h
  height (DelayedImage (extent -> (Z :. _ :. h))) = h
  
  
  ref (VectorImage arr) x y = index arr (Z :. x :. y)
  ref (DelayedImage arr) x y = index arr (Z :. x :. y)
  
  makeImage w h f = DelayedImage . fromFunction (Z :. w :. h) $ g where
    g (Z :. x :. y) = f x y
  
  fromVector w h = VectorImage . (fromUnboxed (Z :. w :. h))
  
  toVector (rCompute -> (VectorImage arr)) = toUnboxed arr

  imageMap = imgMap

  imageZipWith = imgZipWith

  compute =rCompute


imgMap op (PureImage arr) = PureImage $ map op arr
imgMap op img = DelayedImage $ map op $ getInner img

imgZipWith op (PureImage arr) img2 =
  DelayedImage $ map (op (arr ! Z)) (getInner img2)
imgZipWith op img1 (PureImage arr) =
  DelayedImage $ map (flip op (arr ! Z)) (getInner img1)
imgZipWith op img1 img2 =
  DelayedImage $ zipWith op (getInner img1) (getInner img2)

instance (V.Unbox px, Num px) => Num (Image px) where
  (+) = imgZipWith (+)
  (-) = imgZipWith (-)
  (*) = imgZipWith (*)
  abs = imgMap abs
  signum = imgMap signum
  fromInteger i = PureImage $ fromFunction Z (const . fromInteger $ i)

instance (V.Unbox px, Fractional px) => Fractional (Image px) where
  (/) = imgZipWith (/)
  fromRational r = PureImage $ fromFunction Z (const . fromRational $ r)

instance (V.Unbox px, Floating px) => Floating (Image px) where
  pi      = PureImage $ fromFunction Z (const pi)
  exp     = imgMap exp
  log     = imgMap log
  sin     = imgMap sin
  cos     = imgMap cos
  asin    = imgMap asin
  atan    = imgMap atan
  acos    = imgMap acos
  sinh    = imgMap sinh
  cosh    = imgMap cosh
  asinh   = imgMap asinh
  atanh   = imgMap atanh
  acosh   = imgMap acosh




rCompute i@(VectorImage _) = i
rCompute (DelayedImage arr)
  | isSmall arr = VectorImage parr
  | otherwise = VectorImage . computeUnboxedS $ arr
  where [parr] = computeUnboxedP arr



  

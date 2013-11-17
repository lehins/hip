{-# LANGUAGE ViewPatterns, TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Graphics.Image.Internal (
  Image,
  Processable(..)
  ) where

import Prelude hiding (map, zipWith)
import Graphics.Image.Base
import qualified Data.Array.Repa as R (map, zipWith)
import Data.Array.Repa hiding (map, zipWith)

import Prelude hiding (map, zipWith)
import qualified Data.Vector.Unboxed as V

      
data Image px = VectorImage (Array U DIM2 px)
              | DelayedImage (Array D DIM2 px)
              | PureImage (Array D DIM0 px)


isSmall arr = w*h < 150 where (Z :. h  :. w) = extent arr

getInner (VectorImage arr) = delay arr
getInner (DelayedImage arr) = arr

instance Pixel px => Processable Image px where
  width (VectorImage (extent -> (Z :. _ :. w))) = w
  width (DelayedImage (extent -> (Z :. _ :. w))) = w

  height (VectorImage (extent -> (Z :. h :. _))) = h
  height (DelayedImage (extent -> (Z :. h :. _))) = h
  
  
  ref (VectorImage arr) x y = index arr (Z :. y :. x)
  ref (DelayedImage arr) x y = index arr (Z :. y :. x)
  
  make w h f = DelayedImage . fromFunction (Z :. h :. w) $ g where
    g (Z :. y :. x) = f x y
  
  map = imgMap

  zipWith = imgZipWith

  fold = imgFold

  fromVector w h = VectorImage . (fromUnboxed (Z :. h :. w))
  
  toVector (rCompute -> (VectorImage arr)) = toUnboxed arr


imgMap op (PureImage arr) = PureImage $ R.map op arr
imgMap op img = DelayedImage $ R.map op $ getInner img

imgZipWith op (PureImage arr) img2 =
  DelayedImage $ R.map (op (arr ! Z)) (getInner img2)
imgZipWith op img1 (PureImage arr) =
  DelayedImage $ R.map (flip op (arr ! Z)) (getInner img1)
imgZipWith op img1 img2 =
  DelayedImage $ R.zipWith op (getInner img1) (getInner img2)

imgFold op px (getInner -> arr)
  | isSmall arr = foldAllS op px arr
  | otherwise   = head $ foldAllP op px arr

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



  

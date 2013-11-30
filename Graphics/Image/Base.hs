{-# LANGUAGE ViewPatterns, FlexibleInstances, MultiParamTypeClasses #-}

module Graphics.Image.Base (
  Image, 
  Processable(..),
  Pixel(..), -- Convertable(..),
  getDelayed, fromDelayed, getComputed, fromComputed
  ) where

import Graphics.Image.Internal
import Data.Array.Repa as R
import qualified Data.Vector.Unboxed as V

      
data Image px = ComputedImage  (Array U DIM2 px)
              | DelayedImage (Array D DIM2 px)
              | PureImage    (Array D DIM0 px)


instance Pixel px => Processable Image px where
  rows = fst . dims

  cols = snd . dims
  
  dims (DelayedImage arr) = (r, c) where (Z :. r :. c) = extent arr
  dims (ComputedImage arr) = (r, c) where (Z :. r :. c) = extent arr

  ref img r c = index (getComputed img) (Z :. r :. c)
  
  make m n f = DelayedImage . fromFunction (Z :. m :. n) $ g where
    g (Z :. m :. n) = f m n
  
  map = imgMap

  zipWith = imgZipWith

  fold = imgFold

  traverse = imgTraverse

  fromVector r c = ComputedImage . (fromUnboxed (Z :. r :. c))
  
  toVector = toUnboxed . getComputed


isSmall (extent -> (Z :. r  :. c)) = r*c < 150

getDelayed (ComputedImage arr)  = delay arr
getDelayed (DelayedImage arr) = arr

fromDelayed = DelayedImage

getComputed (ComputedImage arr) = arr
getComputed (DelayedImage arr)
  | isSmall arr = head . computeUnboxedP $ arr
  | otherwise = computeUnboxedS arr

fromComputed = ComputedImage . head 

imgMap op (PureImage arr)    = PureImage $ R.map op arr
imgMap op img = DelayedImage . (R.map op) . getDelayed $ img

imgZipWith op (PureImage arr) img2 =
  DelayedImage $ R.map (op (arr ! Z)) (getDelayed img2)
imgZipWith op img1 (PureImage arr) =
  DelayedImage $ R.map (flip op (arr ! Z)) (getDelayed img1)
imgZipWith op img1 img2 =
  DelayedImage $ R.zipWith op (getDelayed img1) (getDelayed img2)

imgFold op px (getDelayed -> arr)
  | isSmall arr = foldAllS op px arr
  | otherwise   = head $ foldAllP op px arr

imgTraverse img f g = DelayedImage $ R.traverse (getDelayed img) f' g' where
  toShape (r, c) = (Z :. r :. c)
  f' (Z :. m :. n) = toShape (f m n)
  g' u (Z :. r1 :. c1) = g (u' u) r1 c1
  u' u r c = u ( Z :. r :. c)

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




  

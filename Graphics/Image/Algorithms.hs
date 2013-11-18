{-# LANGUAGE ViewPatterns #-}
module Graphics.Image.Algorithms where

import Prelude hiding (map, fold, zipWith)
import Graphics.Image.Base (Pixel(..))
import Graphics.Image.Internal
import Data.Complex

getLargest :: (Ord px, Pixel px) => Image px -> px
getLargest img = fold max (ref img 0 0) img

getSmallest :: (Ord px, Pixel px) => Image px -> px
getSmallest img = fold min (ref img 0 0) img

normalize :: (Ord px, Pixel px) => Image px -> Image px
normalize img
  | s == w = img * 0
  | otherwise = map normalizer img
  where (s, w) = (strongest (getLargest img), weakest (getSmallest img))
        normalizer px = (px - w)/(s - w)

ref1 :: Pixel px => Image px -> Double -> Double -> px
ref1 img@(dims -> (w, h)) x y = fx0 + y'*(fx1-fx0) where
  (x0, y0) = (floor x, floor y)
  (x1, y1) = (x0 + 1, y0 + 1)
  x' = pixel (x - (fromIntegral x0))
  y' = pixel (y - (fromIntegral y0))
  f00 = refd (pixel 0) img x0 y0
  f10 = refd (pixel 0) img x1 y0
  f01 = refd (pixel 0) img x0 y1 
  f11 = refd (pixel 0) img x1 y1 
  fx0 = f00 + x'*(f10-f00)
  fx1 = f01 + x'*(f11-f01)

correct img angle ratio = make (mw * round ratio) mh gop
  where alpha = (2*pi)/(mwd*ratio)
        (w, h) = (width img, height img)
        (mw, mh) = (div w 2, div h 2)
        (mwd, mhd) = (fromIntegral mw, fromIntegral mh)
        gop (fromIntegral -> x) (fromIntegral -> y) = ref1 img x' y'
          where z  = mkPolar y (x*alpha)
                x' = imagPart z + mhd
                y' = realPart z + mwd 

{-| Rotate an image by angle `theta` in radians in counterclockwise direction
    while preserving the dimsensions of the original image. Pixels out of bounds are
    replaced with zero (black) pixels.
-}
rotate img@(dims -> (w, h)) theta = make w h gop where
  (mw, mh) = (fromIntegral $ div w 2, fromIntegral $ div h 2)
  gop (fromIntegral -> x) (fromIntegral -> y) = ref1 img x' y' where
    z = exp(0 :+ theta) * ((x-mw) :+ (y-mh))
    x' = mw + realPart z
    y' = mh + imagPart z

{-| Same as `rotate` except the dimensions of the new image are adjusted so that
    the rotated image would completely fit.
-}
rotate' img@(dims -> (w, h)) theta = traverse img f g where
  (nwd, nhd) = (w' * cost + h' * sint, h' * cost + w' * sint)
    where (w', h')     = (fromIntegral w, fromIntegral h)
          (sint, cost) = (sin theta, cos theta)
  f _ _ = (ceiling nwd, ceiling nhd)
  g _ = gop
  (mw, mh) = (fromIntegral w / 2, fromIntegral h / 2)
  gop (fromIntegral -> x) (fromIntegral -> y) = ref1 img x' y' where
    z = exp(0 :+ theta) * ((x-(nwd/2)) :+ (y-(nhd/2)))
    x' = mw + realPart z
    y' = mh + imagPart z

rotate'' img@(dims -> (w, h)) theta = make nw nh gop where
  (nw, nh) = (ceiling nwd, ceiling nhd)
  (nwd2, nhd2) = (nwd/2, nhd/2)
  (nwd, nhd) = (w' * cost + h' * sint, h' * cost + w' * sint)
    where (w', h')     = (fromIntegral w, fromIntegral h)
          (sint, cost) = (sin theta, cos theta)
  (mw, mh) = (fromIntegral w / 2, fromIntegral h / 2)
  gop (fromIntegral -> x) (fromIntegral -> y) = ref1 img x' y' where
    z = exp(0 :+ theta) * ((x - nwd2) :+ (y - nhd2))
    x' = mw + realPart z
    y' = mh + imagPart z


{-# LANGUAGE ViewPatterns, BangPatterns, FlexibleContexts #-}
module Graphics.Image.Processing.Geometric (
  normalize, rotate, rotate'
  ) where

import Prelude hiding (map, zipWith)
import Graphics.Image.Definition
import Data.Complex


{-| Rotate an image by angle `theta` in radians in counterclockwise direction
    while preserving the dimsensions of the original image. Pixels out of bounds are
    replaced with zero (black) pixels.
-}
rotate :: (Concrete Image px, Pixel px) => Image px -> Double -> Image px
rotate img@(dims -> (w, h)) theta = make w h gop where
  (mw, mh) = (fromIntegral $ div w 2, fromIntegral $ div h 2)
  gop (fromIntegral -> x) (fromIntegral -> y) = ref1 img x' y' where
    z = exp(0 :+ theta) * ((x-mw) :+ (y-mh))
    x' = mw + realPart z
    y' = mh + imagPart z

{-| Same as `rotate` except the dimensions of the new image are adjusted so that
    the rotated image will fit completely.
-}
rotate' :: (Concrete Image px, Pixel px) => Image px -> Double -> Image px
rotate' img@(dims -> (w, h)) theta = make nw nh gop where
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

{-
rotate'' :: (Concrete Image px, Pixel px) => Image px -> Double -> Image px
rotate'' img@(dims -> (w, h)) theta = traverse img f g where
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
-}

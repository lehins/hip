{-# LANGUAGE ViewPatterns #-}
module Data.Image.Convolution where

import Data.Image
import Data.Image.Internal
import Data.Image.Color
import Data.Array.Repa
import Data.Image.Pixel
import Data.Maybe
import Data.Array.Repa.Algorithms.Convolve

--convolve i1@(dim -> (w1, h1)) i2@(dim -> (w2, h2)) =

im2arr im@(dim -> (w, h)) = fromUnboxed (Z :. (w::Int) :. (h::Int)) $ toVector im

rotate im@(dim -> (w, h)) =
  maybe im ((fromVector w h) . toUnboxed) $ rot $ im2arr im  where
    rot arr = (computeP $ rot180 arr) :: Maybe (Array U DIM2 Color)

sq = makeImage 3 3 (\_ _ -> (RGB 1 1 1))

convolve im@(dim -> (w, h)) =
  maybe im ((fromVector w h) . toUnboxed) $ rot $ im2arr im  where
    rot arr = (convolveP (const (RGB 1 1 1)) (im2arr sq) arr) :: Maybe (Array U DIM2 Color)



rot180 :: (Source r e) => Array r DIM2 e -> Array D DIM2 e
rot180 g = backpermute e flop g
    where
        e@(Z :. x :. y)   = extent g
        flop (Z :. i         :. j        ) =
             (Z :. x - i - 1 :. y - j - 1)

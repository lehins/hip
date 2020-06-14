-- |
-- Module      : Graphics.Image.Processing.Gabor
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Gabor where

import Graphics.Image.IO as I
import Graphics.Image.Internal as I
import Graphics.ColorSpace
import Graphics.Image.Processing.Convolution


gabor :: RealFloat a =>
         a -- ^ λ
      -> a -- ^ θ -- orientation
      -> a -- ^ ψ
      -> a -- ^ σ
      -> a -- ^ γ
      -> a -- ^ x
      -> a -- ^ y
      -> Complex a
gabor λ θ ψ σ γ x y = exp ( (-0.5) * ((x'^2 + γ^2*y'^2) / (σ^2)) :+ 0) * exp ( 0 :+ (2*pi*x'/λ+ψ) )
    where x' =  x * cos θ + y * sin θ
          y' = -x * sin θ + y * cos θ


imgGabor :: Ix2 -> Image Y (Complex Double)
imgGabor sz@(m :. n) =
  makeImage
    sz
    (\(i :. j) ->
       promote $
       gabor
         3.5
         (pi / 3)
         0
         2.8
         0.3
         (fromIntegral (j - (n `div` 2)))
         (fromIntegral (i - (m `div` 2))))

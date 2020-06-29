-- |
-- Module      : Graphics.Image.Processing.Gabor
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Gabor where

import Graphics.Image.Internal as I
import Graphics.Pixel
import Graphics.Image.Processing.Complex.Internal


gabor :: RealFloat a =>
         a -- ^ λ
      -> a -- ^ θ -- orientation
      -> a -- ^ ψ
      -> a -- ^ σ
      -> a -- ^ γ
      -> a -- ^ x
      -> a -- ^ y
      -> Complex a
gabor λ θ ψ σ γ x y =
  exp
    ((-0.5) *
     ((x' ^ (2 :: Int) + γ ^ (2 :: Int) * y' ^ (2 :: Int)) / (σ ^ (2 :: Int))) :+
     0) *
  exp (0 :+ (2 * pi * x' / λ + ψ))
  where
    x' = x * cos θ + y * sin θ
    y' = -x * sin θ + y * cos θ


imgGabor :: Sz2 -> Image Y (Complex Double)
imgGabor sz@(Sz (m :. n)) =
  makeImage
    sz
    (\(i :. j) ->
       pure $
       gabor
         3.5
         (pi / 3)
         0
         2.8
         0.3
         (fromIntegral (j - (n `div` 2)))
         (fromIntegral (i - (m `div` 2))))

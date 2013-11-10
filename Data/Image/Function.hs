{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module Data.Image.Function where

import Data.Image.Internal
import Data.Image.Pixel

type Function n px = (Num n, Pixel px) => n -> n -> px

gaussian :: Double -> Function Double Gray
gaussian var x y = Gray $ exp (-(x^2+y^2)/(2*var))

convolution w h f g i j =
  [(f m n)*(g (i-m) (j-n)) | m <- [0..w], n <- [0..h]]

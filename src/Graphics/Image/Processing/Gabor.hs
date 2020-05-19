{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-} 

module Graphics.Image.Processing.Gabor where

import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.STRef 

import Prelude as P hiding (subtract)
import Graphics.Image.Processing.Filter
import Graphics.Image.Interface as I
import Graphics.Image
import Graphics.Image.Types as IP
import Graphics.Image.ColorSpace (X)
import Data.Complex (Complex((:+)))

gaborfn
  :: (RealFloat p, Fractional p) => p -> p -> p -> p -> p -> p -> p -> Complex p  
gaborfn λ θ ψ σ γ x y = exp ( (-0.5) * ((x'^2 + γ^2*y'^2) / (σ^2)) :+ 0) * exp ( 0 :+ (2*pi*(x'/λ+ψ)) )
    where x' =  x * cos θ + y * sin θ
          y' = -x * sin θ + y * cos θ
          λ = 10.0
          θ = pi
          ψ = 0 
          σ = 4.0
          γ = 0.5

gaborFilter :: (Array arr cs e, Array arr X e, Floating e, Fractional e) =>
                   Border (Pixel cs e) -- ^ Border resolution technique.
                -> Filter arr cs e
gaborFilter !border =
  Filter (correlate border gV' . correlate border gV)
  where
    !gV = compute $ (gabor / scalar weight)
    !gV' = compute $ transpose gV
    !gabor = makeImage (1, n) (promote gaborfn)
    !weight = I.fold (+) 0 gabor
    !n = 5
    



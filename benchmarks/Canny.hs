{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude as P
import Criterion.Main
import Graphics.Image as I
import Graphics.Image.Interface as I
import Graphics.Image.Interface.Repa

import Data.Array.Repa as R
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2


sobelGx :: I.Array arr cs e => Image arr cs e -> Image arr cs e
sobelGx =
  convolve Edge (fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]])

sobelGx' :: I.Array arr cs e => Image arr cs e -> Image arr cs e
sobelGx' =
  convolveCols Edge [1, 2, 1] . convolveRows Edge [1, 0, -1]

sobelGy :: I.Array arr cs e => Image arr cs e -> Image arr cs e
sobelGy =
  convolve Edge (fromLists [[-1,-2,-1], [ 0, 0, 0], [ 1, 2, 1]])

sobelGy' :: I.Array arr cs e => Image arr cs e -> Image arr cs e
sobelGy' =
  convolveCols Edge [1, 0, -1] . convolveRows Edge [1, 2, 1]


sobelGxR
  :: R.Array U DIM2 (Pixel Y Double)
     -> R.Array PC5 DIM2 (Pixel Y Double)
sobelGxR = mapStencil2 BoundClamp stencil 
  where stencil = makeStencil2 3 3
                  (\ix -> case ix of
                      Z :. -1 :. -1  -> Just (-1)
                      Z :.  0 :. -1  -> Just (-2)
                      Z :.  1 :. -1  -> Just (-1)
                      Z :. -1 :.  1  -> Just 1
                      Z :.  0 :.  1  -> Just 2
                      Z :.  1 :.  1  -> Just 1
                      _              -> Nothing)

sobelGyR
  :: R.Array U DIM2 (Pixel Y Double)
     -> R.Array PC5 DIM2 (Pixel Y Double)
sobelGyR = mapStencil2 BoundClamp stencil 
  where stencil = makeStencil2 3 3
                  (\ix -> case ix of
                      Z :.  1 :. -1  -> Just (-1)
                      Z :.  1 :.  0  -> Just (-2)
                      Z :.  1 :.  1  -> Just (-1)
                      Z :. -1 :. -1  -> Just 1
                      Z :. -1 :.  0  -> Just 2
                      Z :. -1 :.  1  -> Just 1
                      _              -> Nothing)


main :: IO ()
main = do
  img <- readImageY RP "images/frog.jpg"
  let sobel = sqrt (sobelGx img ^ (2 :: Int) + sobelGy img ^ (2 :: Int))
  let sobel' = sqrt (sobelGx' img ^ (2 :: Int) + sobelGy' img ^ (2 :: Int))
  let imgR = toRepaArray img
  let sobelR =
        R.map
          sqrt
          (R.map (^ (2 :: Int)) (sobelGxR imgR) +^
           R.map (^ (2 :: Int)) (sobelGyR imgR))
  defaultMain
    [ bgroup
        "Sobel"
        [ bench "naive" $ whnf compute sobel
        , bench "separated" $ whnf compute sobel'
        , bench "repa" $ whnf (compute . fromRepaArrayP) sobelR
        ]
    ]


-- mkStencil :: (Int, Int) -> ((Int, Int) -> Pixel cs e) -> Image ST cs e
-- mkStencil

{-# LANGUAGE FlexibleContexts #-}

module Main where

import Criterion.Main
import Graphics.Image as I
import Graphics.Image.Processing.Canny as I
import Prelude as P

main :: IO ()
main = do
  imgY <- readImageY "images/downloaded/frog-1280x824.jpg"
  imgRGBD <- readImageRGB "images/downloaded/frog-1280x824.jpg"
  defaultMain
    [ env (return imgRGBD) $ \img ->
        bgroup
          "Gaussian"
          [ bgroup
              "StdDev=derived"
              [ bench "3x3" $ whnf (gaussianBlur3x3 Edge) img
              , bench "5x5" $ whnf (gaussianBlur5x5 Edge) img
              , bench "7x7" $ whnf (gaussianBlur7x7 Edge) img
              , bench "9x9" $ whnf (gaussianBlur 4 Nothing Edge) img
              , bench "11x11" $ whnf (gaussianBlur 5 Nothing Edge) img
              ]
          , bgroup
              "StdDev=1"
              [ bench "3x3" $ whnf (gaussianBlur 1 (Just 1) Edge) img
              , bench "5x5" $ whnf (gaussianBlur 2 (Just 1) Edge) img
              , bench "7x7" $ whnf (gaussianBlur 3 (Just 1) Edge) img
              , bench "9x9" $ whnf (gaussianBlur 4 (Just 1) Edge) img
              , bench "11x11" $ whnf (gaussianBlur 5 (Just 1) Edge) img
              ]
          ]
    , env (return imgRGBD) $ \img ->
        bgroup
          "Average"
          [ bench "3x3" $ whnf (averageBlur3x3 Edge) img
          , bench "5x5" $ whnf (averageBlur5x5 Edge) img
          , bench "7x7" $ whnf (averageBlur7x7 Edge) img
          , bench "9x9" $ whnf (averageBlur 4 Edge) img
          , bench "11x11" $ whnf (averageBlur 5 Edge) img
          ]
    , env (return imgRGBD) $ \img ->
        bgroup
          "Sobel Operator"
          [ bench "Not Normalized" $ whnf (mapFilter Edge sobelOperator) img
          , bench "Normalized" $ whnf (mapFilter Edge sobelOperatorNormal) img
          ]
    -- , env (return imgY) $ \img ->
    --     bgroup
    --       "Sobel Operator Y"
    --       [ bench "No Normalization" $ whnf (applyFilter Edge sobelOperator) img
    --       ]
    , env (return imgY) $ \img ->
        bench "Canny" $ whnf (canny 0.2 0.4) img
    ]

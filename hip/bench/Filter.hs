module Main where

import           Criterion.Main
import           Graphics.Image                   as I
import           Graphics.Image.Processing.Filter
import           Prelude                          as P


main :: IO ()
main = do
  imgRGBD <- readImageAuto "images/frog.jpg" :: IO (Image RGB Double)
  defaultMain
    [ env (return imgRGBD) $ \img ->
        bgroup
          "Gaussian 5x5"
          [ bench "Normal" $ whnf (applyFilter Edge gaussian5x5) img
          , bench "Separated" $ whnf (applyGaussian5x5 Edge) img
          ]
    -- , env (return imgRGBD) $ \img ->
    --     bgroup
    --       "Sobel Operator RGB"
    --       [ bench "No Normalization" $ whnf (applyFilter Edge sobelOperator') img
    --       ]
    -- , env (return $ I.map toPixelY imgRGBD) $ \img ->
    --     bgroup
    --       "Sobel Operator Y"
    --       [ bench "No Normalization" $ whnf (applyFilter Edge sobelOperator) img
    --       ]
    ]

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude as P
import Criterion.Main
import Graphics.Image as I
import Graphics.Image.Interface as I




main :: IO ()
main = do
  let toX (PixelY y) = PixelX y
  let !imgY =
        compute $
        makeImage
          (1024, 768)
          (\(i, j) -> fromIntegral ((min i j) `div` (1 + max i j))) :: Image VU Y Double
  defaultMain
    [ bgroup
        "Histogram"
        [ bench "Compute All" $ nf (hBins . head . getHistograms) imgY
        , bench "Compute Direct " $ nf (hBins . getHistogram . I.map toX) imgY
        , bench "Equalize" $ nf equalizeHistogram imgY
        ]
    ]

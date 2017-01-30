{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude as P
import Criterion.Main
import Graphics.Image as I
import Graphics.Image.Interface as I


superimpose' :: Array arr cs e =>
              (Int, Int)     -- ^ @(i, j)@ starting index from within a source image.
           -> Image arr cs e -- ^ Image to be positioned above the source image.
           -> Image arr cs e -- ^ Source image.
           -> Image arr cs e              
superimpose' !top@(i0, j0) imgS img =
  makeImageWindowed (dims img) (top, bottom) indexSM (unsafeIndex imgM)
  where !imgSM = toManifest imgS
        indexSM !(i, j) = unsafeIndex imgSM (i - i0, j - j0)
        !imgM = toManifest img
        !bottom = (i0 + rows imgS, j0 + cols imgS)

main :: IO ()
main = do
  let !img = compute $ makeImage (100, 100)
              (\(i, j) -> fromIntegral ((min i j) `div` (1 + max i j )))
              :: Image VU Y Word8
  let !imgS = compute $ makeImage (5, 5)
              (\(i, j) -> fromIntegral ((min i j) `div` (1 + max i j )))
              :: Image VU Y Word8
  let superimposeCur = superimpose (10, 10) imgS img
  let superimposeAlt = superimpose' (10, 10) imgS img
  defaultMain
    [ bgroup
        "Superimpose"
        [
          bench "alternative" $ whnf compute superimposeAlt
        , bench "current" $ whnf compute superimposeCur
        ]
    ]




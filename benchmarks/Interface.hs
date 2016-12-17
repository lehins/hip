{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude as P
import Criterion.Main
import Graphics.Image.Interface as I
--import Graphics.Image.Processing
import qualified Graphics.Image.Interface.Repa as R
--import qualified Graphics.Image.Interface.Vector as V
import Graphics.Image.Types

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "RP fusion"
        [ bench "native" $
          nf
            (R.computeP . (noFusion :: (Int, Int) -> Image RD Y Double))
            (1000, 1000)
        , bench "RD fusion" $
          nf
            (R.computeP . (fusion :: (Int, Int) -> Image RD Y Double))
            (1000, 1000)
        , bench "RP no fusion (suspendedComputeP)" $
          nf
            (R.computeP . (fusion :: (Int, Int) -> Image RP Y Double))
            (1000, 1000)
        ]
    , bgroup
        "RS fusion"
        [ bench "native" $
          nf
            (R.computeS . (noFusion :: (Int, Int) -> Image RD Y Double))
            (1000, 1000)
        , bench "RD fusion" $
          nf
            (R.computeS . (fusion :: (Int, Int) -> Image RD Y Double))
            (1000, 1000)
        , bench "RS no fusion" $
          nf
            (R.computeS . (fusion :: (Int, Int) -> Image RS Y Double))
            (1000, 1000)
        ]
    , bgroup
        "VU fusion"
        [ bench "no fusion" $
          nf (noFusion :: (Int, Int) -> Image VU Y Double) (1000, 1000)
        , bench "VU fusion" $
          nf (fusion :: (Int, Int) -> Image VU Y Double) (1000, 1000)
        ]
    ]
--frog <- V.readImageY "images/frog.jpg"
--     [ bgroup
--         ("makeImage big " ++ show bigDims)
--         [ bench "makeImage VU" $ nf (`V.makeImage` getPxY) bigDims
--         , bench "computeS" $ nf R.computeS (R.makeImage bigDims getPxY)
--           -- parallel
--         , bench "computeP" $ nf R.computeP (R.makeImage bigDims getPxY)
--         ]
--     , bgroup
--         "Sobel operator"
--         [ bench "sobel VU" $ nf sobel frog
--         , bench "sobel RS" $ nf (sobel . exchange RS) frog
--           -- parallel
--         , bench "sobel RP" $ nf (sobel . R.computeP . exchange RP) frog
--         ]
--     ]
    where
--     bigDims = (2000, 2000)
      getPxY :: (Int, Int) -> Pixel Y Double
      getPxY (i, j) = fromIntegral (i * j)
      noFusion ds = makeImage ds getPx
        where getPx :: (Int, Int) -> Pixel Y Double
              getPx (i, j) = (getPxY (i, j) / 5 - fromIntegral i) * 21
      fusion ds = imap (\ (i, _) px -> (px - fromIntegral i) * 21) $ (makeImage ds getPxY / 5)


-- sobel :: ManifestArray arr cs Double => Image arr cs Double -> Image arr cs Double
-- sobel img = sqrt (imgX ^ (2 :: Int) + imgY ^ (2 :: Int))
--   where
--     imgX = convolve Edge (fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]]) img
--     imgY = convolve Edge (fromLists [[-1,-2,-1], [ 0, 0, 0], [ 1, 2, 1]]) img

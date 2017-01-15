{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude as P
import Criterion.Main
import Graphics.Image.Interface as I
--import Graphics.Image.Processing

--import qualified Graphics.Image.Interface.Vector as V
import Graphics.Image.Types

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "RPU fusion"
        [ bench "native" $
          whnf
            (compute . (noFusion :: (Int, Int) -> Image RPU RGB Double))
            (1000, 1000)
        , bench "RPU fusion" $
          whnf
            (compute . (fusion :: (Int, Int) -> Image RPU RGB Double))
            (1000, 1000)
        ]
    , bgroup
        "RPU fusion RGBd"
        [ bench "native" $
          whnf
            (compute . (noFusion' :: (Int, Int) -> Image RPU RGBd Double))
            (1000, 1000)
        , bench "RPU fusion" $
          whnf
            (compute . (fusion' :: (Int, Int) -> Image RPU RGBd Double))
            (1000, 1000)
        ]
    , bgroup
        "RSU fusion"
        [ bench "native" $
          whnf
            (compute . (noFusion :: (Int, Int) -> Image RSU RGB Double))
            (1000, 1000)
        , bench "RSU fusion" $
          whnf
            (compute . (fusion :: (Int, Int) -> Image RSU RGB Double))
            (1000, 1000)
        ]
    , bgroup
        "RSU fusion RGBd"
        [ bench "native" $
          whnf
            (compute . (noFusion' :: (Int, Int) -> Image RSU RGBd Double))
            (1000, 1000)
        , bench "RSU fusion" $
          whnf
            (compute . (fusion' :: (Int, Int) -> Image RSU RGBd Double))
            (1000, 1000)
        ]
    , bgroup
        "RPS fusion"
        [ bench "native" $
          whnf
            (compute . (noFusion :: (Int, Int) -> Image RPS RGB Double))
            (1000, 1000)
        , bench "RPS fusion" $
          whnf
            (compute . (fusion :: (Int, Int) -> Image RPS RGB Double))
            (1000, 1000)
        ]
    , bgroup
        "RPS fusion RGBd"
        [ bench "native" $
          whnf
            (compute . (noFusion' :: (Int, Int) -> Image RPSd RGB Double))
            (1000, 1000)
        , bench "RPS fusion" $
          whnf
            (compute . (fusion' :: (Int, Int) -> Image RPS RGBd Double))
            (1000, 1000)
        ]
    , bgroup
        "RSS fusion"
        [ bench "native" $
          whnf
            (compute . (noFusion :: (Int, Int) -> Image RSS RGB Double))
            (1000, 1000)
        , bench "RSS fusion" $
          whnf
            (compute . (fusion :: (Int, Int) -> Image RSS RGB Double))
            (1000, 1000)
        ]
    , bgroup
        "RSS fusion RGBd"
        [ bench "native" $
          whnf
            (compute . (noFusion' :: (Int, Int) -> Image RSS RGBd Double))
            (1000, 1000)
        , bench "RSS fusion" $
          whnf
            (compute . (fusion' :: (Int, Int) -> Image RSS RGB dDouble))
            (1000, 1000)
        ]
    , bgroup
        "VU fusion"
        [ bench "no fusion" $
          nf (noFusion :: (Int, Int) -> Image VU RGB Double) (1000, 1000)
        , bench "VU fusion" $
          nf (fusion :: (Int, Int) -> Image VU RGB Double) (1000, 1000)
        ]
    , bgroup
        "VS fusion"
        [ bench "no fusion" $
          nf (noFusion :: (Int, Int) -> Image VS RGB Double) (1000, 1000)
        , bench "VS fusion" $
          nf (fusion :: (Int, Int) -> Image VS RGB Double) (1000, 1000)
        ]
    ]
--frog <- V.readImageRGB "images/frog.jpg"
--     [ bgroup
--         ("makeImage big " ++ show bigDims)
--         [ bench "makeImage VU" $ nf (`V.makeImage` getPxRGB) bigDims
--         , bench "computeS" $ nf R.computeS (R.makeImage bigDims getPxRGB)
--           -- parallel
--         , bench "computeP" $ nf R.computeP (R.makeImage bigDims getPxRGB)
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
      getPxRGB :: (Int, Int) -> Pixel RGB Double
      getPxRGB (i, j) = fromIntegral (i * j)
      noFusion ds = makeImage ds getPx
        where getPx :: (Int, Int) -> Pixel RGB Double
              getPx (i, j) = (getPxRGB (i, j) / 5 - fromIntegral i) * 21
      fusion ds = imap (\ (i, _) px -> (px - fromIntegral i) * 21) $ (makeImage ds getPxRGB / 5)
      getPxRGB' :: (Int, Int) -> Pixel RGBd Double
      getPxRGB' (i, j) = fromIntegral (i * j)
      noFusion' ds = makeImage ds getPx
        where getPx :: (Int, Int) -> Pixel RGBd Double
              getPx (i, j) = (getPxRGB' (i, j) / 5 - fromIntegral i) * 21
      fusion' ds = imap (\ (i, _) px -> (px - fromIntegral i) * 21) $ (makeImage ds getPxRGB' / 5)



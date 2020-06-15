{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Codec.Picture.Extra
import Criterion.Main
import qualified Data.Massiv.Array.IO as M
import Graphics.Image as I
import Prelude as P

main :: IO ()
main = do
  let down = Sz (75 :. 240)
      up = Sz (125 :. 400)
  defaultMain
    [ env (readImageRGB8 "images/frog.jpg") $ \img ->
        bgroup
          "Resize"
          [ benchBilinearResize down img
          , benchBilinearResize up img
          , benchBicubicResize down img
          , benchBicubicResize up img
          ]
    ]


benchBilinearResize :: Sz2 -> Image (SRGB 'Linear) Word8 -> Benchmark
benchBilinearResize sz@(Sz2 m n) ~img@(Image a) =
  bgroup
    ("Bilinear " ++ show sz)
    [ bench "HIP" $ nf (resize Bilinear (Fill 0) sz) i
    , bench "JuicyPixels-extra" $ nf (scaleBilinear n m) jp
    ]
  where
    i = I.map (fmap toFloat) img
    jp = M.toJPImageRGB8 (M.toImageBaseModel a)

benchBicubicResize :: Sz2 -> Image (SRGB 'Linear) Word8 -> Benchmark
benchBicubicResize sz img =
  bgroup
    ("Bcubic " ++ show sz)
    [bench "HIP" $ nf (resize (Bicubic 0.5) (Fill 0) sz) i]
  where
    i = I.map (fmap toFloat) img

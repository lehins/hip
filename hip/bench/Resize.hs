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
  let down2x2 = Sz (3300 :. 2100)
      down4x4 = Sz (1650 :. 1050)
      down = Sz (75 :. 240)
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
    , env (readImageRGB "images/downloaded/frog-1280x824.jpg") $ \img ->
        bgroup
          "Shrink2x2"
          [ bench "resize" $ nf (resize Bilinear (Fill 0) down2x2) img
          , bench "resizeDW" $ nf (resizeDW Bilinear (Fill 0) down2x2) img
          , bench "shrink2x2" $ nf shrink2x2 img
          , bench "shrinkVertical . shrinkHorizontal" $
            nf (shrinkVertical 2 . shrinkHorizontal 2) img
          ]
    , env (readImageRGB "/home/lehins/tmp/frog-6600x4200.jpg") $ \img ->
        bgroup
          "Shrink4x4"
          [ bench "resize" $
            nf
              (resize Bilinear (Fill 0) down4x4 .
               resize Bilinear (Fill 0) down2x2)
              img
          , bench "resizeDW" $
            nf
              (resizeDW Bilinear (Fill 0) down4x4 .
               resizeDW Bilinear (Fill 0) down2x2)
              img
          , bench "shrink2x2 . shrink2x2" $ nf (shrink2x2 . shrink2x2) img
          , bench "shrink4x1 . shrink1x4" $ nf (shrink4x1 . shrink1x4) img
          , bench "shrinkVertical . shrinkHorizontal" $
            nf (shrinkVertical 4 . shrinkHorizontal 4) img
          ]
    ]



benchBilinearResize :: Sz2 -> Image RGB Word8 -> Benchmark
benchBilinearResize sz@(Sz2 m n) ~img@(Image a) =
  bgroup
    ("Bilinear " ++ show sz)
    [ bench "HIP" $ nf (resize Bilinear (Fill 0) sz) i
    , bench "JuicyPixels-extra" $ nf (scaleBilinear n m) jp
    ]
  where
    i = I.map (fmap toFloat) img
    jp = M.toJPImageRGB8 a

benchBicubicResize :: Sz2 -> Image RGB Word8 -> Benchmark
benchBicubicResize sz img =
  bgroup
    ("Bicubic " ++ show sz)
    [bench "HIP" $ nf (resize (Bicubic 0.5) (Fill 0) sz) i]
  where
    i = I.map (fmap toFloat) img

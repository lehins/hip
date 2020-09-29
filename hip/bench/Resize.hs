{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Codec.Picture as JP
import Codec.Picture.Extra as JP
import Criterion.Main
import qualified Data.Massiv.Array.IO as M
import Graphics.Image as I
import Prelude as P
import qualified Data.Massiv.Array.IO as A

main :: IO ()
main = do
  let down2x2 = Sz (3300 :. 2100)
      down4x4 = Sz (1650 :. 1050)
      down = Sz (75 :. 240)
      up = Sz (125 :. 400)
      addJP :: Image (SRGB 'Linear) Double -> (Image (SRGB 'Linear) Float, JP.Image JP.PixelRGB8)
      addJP img = (I.map (fmap toFloat) img, M.toJPImageRGB8 $ A.toImageBaseModel a)
        where
          Image a = I.map (fmap toWord8) img
  defaultMain
    [ env (addJP <$> readImageRGB "images/frog.jpg") $ \img ->
        bgroup
          "Resize"
          [ benchBilinearResize down img
          , benchBilinearResize up img
          , benchBicubicResize down (fst img)
          , benchBicubicResize up (fst img)
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
            nf (resize Bilinear (Fill 0) down4x4 . resize Bilinear (Fill 0) down2x2) img
          , bench "resizeDW" $
            nf (resizeDW Bilinear (Fill 0) down4x4 . resizeDW Bilinear (Fill 0) down2x2) img
          , bench "shrink2x2 . shrink2x2" $ nf (shrink2x2 . shrink2x2) img
          , bench "shrink4x1 . shrink1x4" $ nf (shrink4x1 . shrink1x4) img
          , bench "shrinkVertical . shrinkHorizontal" $
            nf (shrinkVertical 4 . shrinkHorizontal 4) img
          ]
    ]



benchBilinearResize :: Sz2 -> (Image (SRGB 'Linear) Float, JP.Image JP.PixelRGB8) -> Benchmark
benchBilinearResize sz@(Sz2 m n) ~(i, jp) =
  bgroup
    ("Bilinear " ++ show sz)
    [ bench "HIP" $ nf (resize Bilinear (Fill 0) sz) i
    , bench "JuicyPixels-extra" $ nf (scaleBilinear n m) jp
    ]

benchBicubicResize :: Sz2 -> Image (SRGB 'Linear) Float -> Benchmark
benchBicubicResize sz img =
  bgroup
    ("Bicubic " ++ show sz)
    [bench "HIP" $ nf (resize (Bicubic 0.5) (Fill 0) sz) img]

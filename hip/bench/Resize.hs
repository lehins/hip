{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Codec.Picture.Extra
import Criterion.Main
import qualified Data.Massiv.Array.IO as M
import Graphics.Image as I
import Graphics.Image.Processing.Filter
import Prelude as P

main :: IO ()
main = do
  let down = Sz (75 :. 240)
      up = Sz (125 :. 400)
  defaultMain
    [ env (readImageRGB8 "images/frog.jpg") $ \img ->
        bgroup "Resize" [benchScale down img, benchScale up img]
    ]


benchScale :: Sz2 -> Image (SRGB 'Linear) Word8 -> Benchmark
benchScale sz@(Sz2 m n) ~img@(Image a) =
  bgroup
    ("Bilinear " ++ show sz)
    [ bench "HIP" $ nf (resize Bilinear (Fill 0) sz) img
    , bench "JuicyPixels-extra" $ nf (scaleBilinear n m) jp
    ]
  where
    jp = M.toJPImageRGB8 (M.toImageBaseModel a)

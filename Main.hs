{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude hiding (map, zipWith)
import Graphics.Image.Repa
--import Graphics.Image.Processing (convolve')
import Graphics.Image.Repa.Parallel
--import Graphics.Image.Processing
  
main :: IO ()  
main = do
  setDisplayProgram "gpicview"
  lena <- readColorImage "lena.jpg"
 --let mask = make (rows lena) (cols lena) (\i j -> if i > j then on else off)
  --writeImage "lena_rot.png" (rotate' (compute lena) (pi/6)) []
  --writeImage "lena_rot.png" (rotate Bilinear lena 1 (pi/3)) []
  --writeImage "lena_conv.png" (convolve pad2 lena) []
  --writeImage "lena_conv'.png" (convolve pad2 lena) []
  display (resize Bilinear 256 1024 lena)
  return ()

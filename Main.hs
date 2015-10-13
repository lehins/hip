{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude hiding (map, zipWith)
import Graphics.Image
--import Graphics.Image.Processing (convolve')
import Graphics.Image.Parallel
--import Graphics.Image.Processing
  
main :: IO ()  
main = do
  setDisplayProgram "gpicview"
  lena <- readColorImage "lena.jpg"
  pad1  <- readColorImage "circular-pad.gif" 
 --let mask = make (rows lena) (cols lena) (\i j -> if i > j then on else off)
  --writeImage "lena_rot.png" (rotate' (compute lena) (pi/6)) []
  --writeImage "lena_rot.png" (rotate Bilinear lena 1 (pi/3)) []
  let !pad2 = scale Bilinear 0.2 pad1
  --writeImage "lena_conv.png" (convolve pad2 lena) []
  writeImage "lena_conv'.png" (convolve pad2 lena) []
  --display ((convert (map (<0.5) lena)) * lena)
  return ()

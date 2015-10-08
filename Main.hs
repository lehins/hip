module Main where

import Graphics.Image
import Graphics.Image.Sequential
import Graphics.Image.Processing.Geometric
  
main :: IO ()  
main = do
  setDisplayProgram "gpicview"
  lena <- readColorImage "lena.jpg"
  --writeImage "lena_rot.png" (rotate' (compute lena) (pi/6)) []
  writeImage "lena_rot.png" (rotate Bilinear lena 1 (pi/3)) []
  --display (rotate'' lena) (pi/6))

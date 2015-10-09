module Main where

import Graphics.Image
import Graphics.Image.Parallel
  
main :: IO ()  
main = do
  setDisplayProgram "gpicview"
  lena <- readColorImage "lena.jpg"
  --writeImage "lena_rot.png" (rotate' (compute lena) (pi/6)) []
  --writeImage "lena_rot.png" (rotate Bilinear lena 1 (pi/3)) []
  display (transpose lena)

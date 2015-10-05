module Main where

import Graphics.Image
import Graphics.Image.Parallel
import Graphics.Image.Processing.Geometric
  
main :: IO ()  
main = do
  setDisplayProgram "gpicview"
  lena <- readColorImage "lena.jpg"
  --writeImage "lena_rot.png" (rotate' (compute lena) (pi/6)) []
  display (rotate' (compute lena) (pi/6))
  display lena

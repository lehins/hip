module Main where

import Graphics.Image.IO
import Graphics.Image.Parallel
import Graphics.Image.Processing.Geometric
  
main :: IO ()  
main = do
  lena <- readGrayImage "lena.jpg"
  writeImage "lena_rot.png" (compute $ rotate (compute lena) (pi/6)) []

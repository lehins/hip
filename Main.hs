module Main where

import Prelude hiding (map)
import Graphics.Image
import Graphics.Image.Pixel
import Graphics.Image.Parallel
--import Graphics.Image.Processing
  
main :: IO ()  
main = do
  setDisplayProgram "gpicview"
  lena <- readColorImage "lena.jpg"
  lena1 <- readGrayImage "lena.jpg"
  --writeImage "lena_rot.png" (rotate' (compute lena) (pi/6)) []
  --writeImage "lena_rot.png" (rotate Bilinear lena 1 (pi/3)) []
  --display (convolve (fromLists [[0.05, 0.25, 0.4, 0.25, 0.05]]) lena)
  display (lena * (map convert lena1))

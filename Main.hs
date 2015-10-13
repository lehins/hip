module Main where

import Prelude hiding (map, zipWith)
import Graphics.Image
import Graphics.Image.Parallel
--import Graphics.Image.Processing
  
main :: IO ()  
main = do
  setDisplayProgram "gpicview"
  lena <- readColorImage "lena.jpg"
  let mask = make (rows lena) (cols lena) (\i j -> if i > j then on else off)
  --writeImage "lena_rot.png" (rotate' (compute lena) (pi/6)) []
  --writeImage "lena_rot.png" (rotate Bilinear lena 1 (pi/3)) []
  --display (convolve (fromLists [[0.05, 0.25, 0.4, 0.25, 0.05]]) lena)
  display ((convert mask) * lena)
  return ()

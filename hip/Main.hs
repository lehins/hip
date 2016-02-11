module Main where

import Prelude hiding (map)
import Graphics.Image.Interface
import Graphics.Image.IO
import Graphics.Image.ColorSpace
import Graphics.Image.Repa

main :: IO ()
main = do
  frog <- readImageRGB "frog.png"
  let im = (map (/ 4) . map (* 2) $ frog)
  let frog1 = computeP im
  --writeImageExact BMP [] "frog1.bmp" frog1
  displayImage frog1
  return ()

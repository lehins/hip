module Main where

import Prelude hiding (map)
import Graphics.Image.Interface
import Graphics.Image.IO
import Graphics.Image.ColorSpace

main :: IO ()
main = do
  frog <- readImageRGB "frog.png"
  let im = (map (/ 4) . map (* 2) $ frog)
  --writeImageExact BMP [] "frog1.bmp" im
  displayImage im
  return ()

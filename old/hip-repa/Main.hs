module Main where

import Graphics.Image.Repa
import Graphics.Image.Repa.Parallel


main :: IO ()
main = do
  frog <- readImageRGB "frog.png"
  writeImage [] "frog1.png" (mult frog frog)

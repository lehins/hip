module Main where

import Graphics.Image.Interface
import Graphics.Image.IO
import Graphics.Image.Repa.Internal

main = do
  frog <- readImageRGB "frog.png"
  let frog1 = computeP frog
  writeImage "frog1.png" (frog1 |*| frog1)

module Main where

import Graphics.Image.Interface
import Graphics.Image.IO
import Graphics.Image.Repa.Internal

main = do
  eye <- readImageRGB "eye.png"
  let eye1 = computeP eye
  writeImage "eye1.png" (eye1 |*| eye1)

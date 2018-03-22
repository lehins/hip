module Main where

import Graphics.Image as I



main :: IO ()
main = do
  img <- readImageAuto "images/frog.jpg" ::  IO (Image RGB Double)
  writeImage "foo.png" $ transpose img


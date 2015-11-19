{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude hiding (map, zipWith)
import qualified Graphics.Image.Repa.Parallel as R
  
main :: IO ()  
main = do
  frog <- R.readImageRGBA "frog.jpg"
  R.writeImage "frog_mod.jpg" frog []
  --R.displayImage frog
  return ()

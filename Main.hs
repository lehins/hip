{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude hiding (map, zipWith)
import qualified Graphics.Image.Repa.Parallel as R
  
main :: IO ()  
main = do
  frog <- R.readImageRGBA "images/downloaded/frog-640x412.jpg"
  centaurus <- R.readImageRGBA "images/downloaded/centaurus-galaxy.jpg"
  cluster <- R.readImageRGBA "images/downloaded/star-cluster.jpg"  
  --R.writeImage "frog_mod.jpg" frog []
  R.displayImage cluster
  return ()

{-# LANGUAGE FlexibleInstances #-}
module Main where

import Graphics.Image.IO
--import Graphics.Image.Gray
--import Graphics.Image.Internal (make, ref)
import qualified Graphics.Image.Parallel as P
--import qualified Graphics.Image.Sequential as S
import Graphics.Image.Processing.Geometric
  
main :: IO ()  
main = do
  lena <- readColorImage "lena.jpg"
  P.writeImage "lena_rot.png" (rotate' (P.compute lena) (pi/6)) []

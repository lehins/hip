{-# LANGUAGE FlexibleInstances #-}
module Main where

import Graphics.Image.IO
import qualified Graphics.Image.Parallel as P
--import qualified Graphics.Image.Sequential as S
import Graphics.Image.Processing.Geometric
  
main :: IO ()  
main = do
  lena <- readGrayImage "lena.jpg"
  P.writeImage "lena_rot.png" (P.compute $ rotate (P.compute lena) (pi/6)) []

{-# LANGUAGE BangPatterns, TypeSynonymInstances #-}
module Main where

import Prelude hiding (map, zipWith)
import Graphics.Image.Repa
import Graphics.Image.Repa.Pixel
--import qualified Graphics.Image.Repa.Fusion as F
--import qualified Graphics.Image.Repa.Sequential as S
import Graphics.Image.Repa.Parallel
--import Graphics.Image.Processing (convolve')
--import Graphics.Image.Processing
  
main :: IO ()  
main = do
  setDisplayProgram "gpicview"
  lena <- readImageRGBA "deathclaw.png"
  display $ transpose lena
  --let pad2 = F.scale Bilinear 0.2 pad1
 --let mask = make (rows lena) (cols lena) (\i j -> if i > j then on else off)
  --writeImage "lena_rot.png" (rotate' (compute lena) (pi/6)) []
  --writeImage "lena_rot.png" (rotate Bilinear lena 1 (pi/3)) []
  --writeImage "lena_conv_hip.png" (convolve pad1 lena) []
  --writeImage "lena_conv'.png" (convolve Wrap (compute pad2) (compute lena)) []
  
  return ()

{-
main :: IO ()  
main = do
  setDisplayProgram "gpicview"
  lena <- F.readColorImage "lena.ppm"
  pad1 <- F.readColorImage "pad.ppm"
  --let pad2 = F.scale Bilinear 0.2 pad1
 --let mask = make (rows lena) (cols lena) (\i j -> if i > j then on else off)
  --writeImage "lena_rot.png" (rotate' (compute lena) (pi/6)) []
  --writeImage "lena_rot.png" (rotate Bilinear lena 1 (pi/3)) []
  writeImage "lena_conv_hip.png" (convolve pad1 lena) []
  --writeImage "lena_conv'.png" (convolve Wrap (compute pad2) (compute lena)) []
  
  return ()

-}

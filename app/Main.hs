{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where

import           Control.Monad

import           Graphics.Image                      as I
import           Graphics.Image.Internal             as I
import           Graphics.Image.Interface.Massiv
import           Prelude                             as P

main :: IO ()
main = do
  let !imgPU =
        compute $
        makeImage
          (600, 600)
          (\(i, j) -> fromIntegral ((min i j) `div` (1 + max i j))) :: Image PU Y Word16
  print (applyFilter (sobelFilter Horizontal Edge) imgPU)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude as P
import Criterion.Main
import Graphics.Image as I
import Graphics.Image.Interface as I
import Graphics.Image.Interface.Vector

import qualified Data.Vector.Unboxed as V


toRGBd :: Pixel RGB Word16 -> Pixel RGBd Word16
toRGBd (PixelRGB r g b) = PixelRGBd r g b
    

main :: IO ()
main = do
  let fI !img = ((2 + img) * 2) - img
  let fI' !img = I.zipWith (-) (I.map (*2) (I.map (2 +) img)) img
  let fV !img = V.zipWith (-) (V.map (*2) (V.map (2 +) img)) img
  let n = 32000
  let !ls = (P.map fromIntegral [1 .. n]) :: [Pixel RGB Word16]
  let !vec = V.fromList ls
  let !img = compute $ fromUnboxedVector (1, n) vec -- I.fromListsR VU [ls]
  let !ls' = (P.map fromIntegral [1 .. n]) :: [Pixel RGBd Word16]
  let !vec' = V.fromList ls'
  let !img' = fromUnboxedVector (1, n) vec' --I.fromListsR VU [ls]
  defaultMain
    [ bgroup
        "Sum U (Image RGB)"
        [ bench "fuse RGB" $ whnf fI img
        , bench "no-fuse RGB" $ whnf fI' img
        , bench "fuse RGBd" $ whnf fI img'
        ]
    , bgroup
        "Sum U (Vector RGB)"
        [ bench "fuse RGB" $ whnf fV vec
        , bench "fuse RGBd" $ whnf fV vec'
        ]
    ]

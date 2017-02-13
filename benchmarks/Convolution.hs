{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude as P
import Criterion.Main
import Control.Monad
import Graphics.Image as I
import Graphics.Image.Interface as I
import Graphics.Image.Interface.Repa

import Data.Array.Repa as R
import Data.Array.Repa.Eval as R
import Data.Array.Repa.Repr.Unboxed as R
import Data.Array.Repa.Stencil as R
import Data.Array.Repa.Stencil.Dim2 as R
import Data.Array.Repa.Algorithms.Convolve as R

-- | Convolution
sobelGx :: I.Array arr cs e => Image arr cs e -> Image arr cs e
sobelGx =
  convolve Edge (fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]])


-- | Same convolution by separable
sobelGxSep :: I.Array arr cs e => Image arr cs e -> Image arr cs e
sobelGxSep =
  convolveCols Edge [1, 2, 1] . convolveRows Edge [1, 0, -1]


-- | Repa algorithms convolution implementation
sobelGxRAlg :: (Unbox e, Num e, Monad m) => R.Array U DIM2 e -> m (R.Array U DIM2 e)
sobelGxRAlg =
  convolveOutP outClamp (R.fromListUnboxed (Z :. 3 :. 3) [-1, 0, 1, -2, 0, 2, -1, 0, 1])

-- | Repa algorithms convolution implementation - separable
sobelGxRAlgSep :: (Unbox e, Num e, Monad m) => R.Array U DIM2 e -> m (R.Array U DIM2 e)
sobelGxRAlgSep =
  convolveOutP
    outClamp
    (R.fromListUnboxed (Z :. 1 :. 3) [1, 0, -1]) >=>
  convolveOutP
    outClamp
    (R.fromListUnboxed (Z :. 3 :. 1) [1, 2, 1])


-- |  Repa stencil base convolution
sobelGxR
  :: (Source r e, Num e) => R.Array r DIM2 e
     -> R.Array PC5 DIM2 e
sobelGxR = mapStencil2 BoundClamp stencil
  where stencil = makeStencil2 3 3
                  (\ix -> case ix of
                      Z :. -1 :. -1  -> Just (-1)
                      Z :.  0 :. -1  -> Just (-2)
                      Z :.  1 :. -1  -> Just (-1)
                      Z :. -1 :.  1  -> Just 1
                      Z :.  0 :.  1  -> Just 2
                      Z :.  1 :.  1  -> Just 1
                      _              -> Nothing)

force
  :: (Load r1 sh e, Unbox e, Monad m)
  => R.Array r1 sh e -> m (R.Array U sh e)
force arr = do
    forcedArr <- computeUnboxedP arr
    forcedArr `deepSeqArray` return forcedArr



main :: IO ()
main = do
  let !imgU = compute $ makeImage (1024, 768)
              (\(i, j) -> fromIntegral ((min i j) `div` (1 + max i j )))
              :: Image RPU Y Double
  let sobelU = sobelGx imgU
  let sobelSepU = sobelGxSep imgU
  let !imgR = toRepaArray imgU
  let sobelR = sobelGxR imgR
  let sobelRAlg = sobelGxRAlg imgR
  let sobelRAlgSep = sobelGxRAlgSep imgR
  defaultMain
    [ bgroup
        "Convolution"
        [
          bench "naive U" $ whnf compute sobelU
        , bench "separated U" $ whnf compute sobelSepU
        , bench "repa U Agorithms" $ whnfIO sobelRAlg
        , bench "repa U Agorithms Separated" $ whnfIO sobelRAlgSep
        , bench "repa U Stencil" $ whnfIO (force sobelR)
        ]
    ]


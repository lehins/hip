{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main where

import           Control.Monad
import           Criterion.Main
import           Data.Array.Repa                     as R
import           Data.Array.Repa.Algorithms.Convolve as R
import           Data.Array.Repa.Eval                as R
import           Data.Array.Repa.Repr.Unboxed        as R
import           Data.Array.Repa.Stencil             as R
import           Data.Array.Repa.Stencil.Dim2        as R

import           Graphics.Image                      as I
import           Graphics.Image.Interface            as I
import           Graphics.Image.Interface.Repa
import           Prelude                             as P

-- | Convolution
sobelGx :: (I.Array arr X e, I.Array arr cs e) => Image arr cs e -> Image arr cs e
sobelGx =
  convolve Edge (fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]])


-- | Same convolution by separable
sobelGxSep :: (I.Array arr X e, I.Array arr cs e) => Image arr cs e -> Image arr cs e
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
                      Z :. -1 :. -1 -> Just (-1)
                      Z :.  0 :. -1 -> Just (-2)
                      Z :.  1 :. -1 -> Just (-1)
                      Z :. -1 :.  1 -> Just 1
                      Z :.  0 :.  1 -> Just 2
                      Z :.  1 :.  1 -> Just 1
                      _             -> Nothing)

forceP
  :: (Load r1 sh e, Unbox e, Monad m)
  => R.Array r1 sh e -> m (R.Array U sh e)
forceP !arr = do
    forcedArr <- computeUnboxedP arr
    forcedArr `deepSeqArray` return forcedArr

forceS
  :: (Load r1 sh e, Unbox e, Monad m)
  => R.Array r1 sh e -> m (R.Array U sh e)
forceS !arr = do
    forcedArr <- return $ computeS arr
    forcedArr `deepSeqArray` return forcedArr



main :: IO ()
main = do
  let !imgU = compute $ makeImage (1600, 1600)
              (\(i, j) -> fromIntegral ((min i j) `div` (1 + max i j )))
              :: Image RPU Y Double
  let !imgU' =
        compute $
        makeImage
          (1600, 1600)
          (\(i, j) -> fromIntegral ((min i j) `div` (1 + max i j))) :: Image VU Y Double
  let !sobelFSep = applyFilter (sobelFilter Horizontal Edge) imgU
  let !sobelFSep' = applyFilter (sobelFilter Horizontal Edge)
  let !imgR = toRepaArray imgU
  let sobelR = sobelGxR imgR
  let sobelRAlgSep = sobelGxRAlgSep imgR
  defaultMain
    [ bgroup
        -- "Gaussian"
        -- [ bench "Native VU" $ whnf (applyFilter gb) imgU'
        -- ]
        "Sobel"
        [
        --   bench "naive U" $ whnf compute sobelU
          bench "repa R Stencil" $ whnfIO (forceP sobelR)
        , bench "Filter R" $ whnf compute sobelFSep
        , bench "Filter VU" $ whnf sobelFSep' imgU'
        , bench "repa R Agorithms Separated" $ whnfIO sobelRAlgSep
        ]
    ]

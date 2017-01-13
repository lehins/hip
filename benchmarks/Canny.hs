{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude as P
import Criterion.Main
import Graphics.Image as I
import Graphics.Image.Interface as I
import Graphics.Image.Interface.Repa


import Data.Array.Repa as R
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2


sobelGx :: I.Array arr cs e => Image arr cs e -> Image arr cs e
sobelGx =
  convolve Edge (fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]])

sobelGy :: I.Array arr cs e => Image arr cs e -> Image arr cs e
sobelGy =
  convolve Edge (fromLists [[-1,-2,-1], [ 0, 0, 0], [ 1, 2, 1]])

-- sobelSGx :: (Exchangable arr VS, I.Array arr cs e, I.Array VS cs e) => Image arr cs e -> Image arr cs e
-- sobelSGx =
--   convolveSparse Edge (fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]])

-- sobelSGy :: (Exchangable arr VS, I.Array arr cs e, I.Array VS cs e) => Image arr cs e -> Image arr cs e
-- sobelSGy =
--   convolveSparse Edge (fromLists [[-1,-2,-1], [ 0, 0, 0], [ 1, 2, 1]])


-- sobelSGx' :: (Exchangable arr VS, I.Array arr cs e, I.Array VS cs e) => Image arr cs e -> Image arr cs e
-- sobelSGx' =
--   convolveSparse Edge (fromLists [[1], [2], [1]]) . convolveSparse Edge (fromLists [[1, 0, -1]])

sobelGx' :: I.Array arr cs e => Image arr cs e -> Image arr cs e
sobelGx' =
  convolveCols Edge [1, 2, 1] . convolveRows Edge [1, 0, -1]

sobelGy' :: I.Array arr cs e => Image arr cs e -> Image arr cs e
sobelGy' =
  convolveCols Edge [1, 0, -1] . convolveRows Edge [1, 2, 1]


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

sobelGyR
  :: (Source r e, Num e) => R.Array r DIM2 e
     -> R.Array PC5 DIM2 e
sobelGyR = mapStencil2 BoundClamp stencil 
  where stencil = makeStencil2 3 3
                  (\ix -> case ix of
                      Z :.  1 :. -1  -> Just (-1)
                      Z :.  1 :.  0  -> Just (-2)
                      Z :.  1 :.  1  -> Just (-1)
                      Z :. -1 :. -1  -> Just 1
                      Z :. -1 :.  0  -> Just 2
                      Z :. -1 :.  1  -> Just 1
                      _              -> Nothing)

force
  :: (Load r1 sh e, Unbox e, Monad m)
  => R.Array r1 sh e -> m (R.Array U sh e)
force arr = do
    forcedArr <- computeUnboxedP arr
    forcedArr `deepSeqArray` return forcedArr

main :: IO ()
main = do
  img' <- readImageY RP "images/downloaded/frog-1280x824.jpg"
  let !img = compute img'
  let sobel = sobelGx img
  let sobelSep = sobelGx' img
  -- let sobelSepVS = sobelSGx' img
  -- let sobelVS = sobelSGx img
  -- let sobelMS = sobelMSGx img
  -- let sobelIMS = sobelIMSGx img
  -- let sobelHMS = sobelHMSGx img
  let imgR = toRepaArray img
  imgRDouble <- force $ R.map (`getPxCh` Y) imgR
  let sobelR = sobelGxR imgR
  let sobelRDouble = sobelGxR imgRDouble
  defaultMain
    [ bgroup
        "Sobel"
        [ bench "naive" $ whnf compute sobel
        , bench "separated" $ whnf compute sobelSep
        -- , bench "separated VS" $ whnf compute sobelSepVS
        -- , bench "sparse VS" $ whnf compute sobelVS
        -- , bench "sparse MS" $ whnf compute sobelMS
        -- , bench "sparse IMS" $ whnf compute sobelIMS
        -- , bench "sparse HMS" $ whnf compute sobelHMS
        --, bench "repa" $ whnf (compute . fromRepaArrayP) sobelR
        , bench "repa Y" $ whnfIO (force sobelR)
        , bench "repa Double" $ whnfIO (force sobelRDouble)
        ]
    ]
  -- img' <- readImageY RS "images/downloaded/frog-1280x824.jpg"
  -- let !imgR = compute img'
  -- let !imgV = toManifest imgR
  -- -- let sobel = sobelGx imgV
  -- -- let sobel' = sobelGx' imgV
  -- -- let sobel'' = sobelSGx imgV
  -- let arrR = toRepaArray imgR
  -- let sobelR = sobelGxR arrR
  -- defaultMain
  --   [ bgroup
  --       "Sobel"
  --       [ bench "naive" $ nf sobelGx imgV
  --       , bench "separated" $ nf sobelGx' imgV
  --       , bench "sparse" $ nf sobelSGx imgV
  --       --, bench "repa" $ whnf (compute . fromRepaArrayP) sobelR
  --       , bench "repa" $ whnfIO (force sobelR)
  --       ]
  --   ]

  -- let sobel = sqrt (sobelGx img ^ (2 :: Int) + sobelGy img ^ (2 :: Int))
  -- let sobel' = sqrt (sobelGx' img ^ (2 :: Int) + sobelGy' img ^ (2 :: Int))
  -- let sobel'' = sqrt (sobelSGx img ^ (2 :: Int) + sobelSGy img ^ (2 :: Int))
  -- let sobel''' = sqrt (sobelMSGx img ^ (2 :: Int) + sobelMSGy img ^ (2 :: Int))
  -- let imgR = toRepaArray img
  -- let sobelR =
  --       R.map
  --         sqrt
  --         (R.map (^ (2 :: Int)) (sobelGxR imgR) +^
  --          R.map (^ (2 :: Int)) (sobelGyR imgR))
  -- defaultMain
  --   [ bgroup
  --       "Sobel"
  --       [ bench "naive" $ whnf compute sobel
  --       , bench "separated" $ whnf compute sobel'
  --       , bench "sparse VS" $ whnf compute sobel''
  --       , bench "sparse MS" $ whnf compute sobel'''
  --       --, bench "repa" $ whnf (compute . fromRepaArrayP) sobelR
  --       , bench "repa" $ whnfIO (force sobelR)
  --       ]
  --   ]


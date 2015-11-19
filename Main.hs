{-# LANGUAGE FlexibleContexts, BangPatterns, ViewPatterns #-}
module Main where

import Prelude hiding (map, zipWith, (++))
import Graphics.Image.Repa.Pixel
import qualified Graphics.Image.Repa as I
--import qualified Graphics.Image.Repa.Sequential as I
import qualified Graphics.Image.Repa.Parallel as I
import qualified Data.Vector.Unboxed as V

import Data.Array.Repa

gradient :: Int -> Int -> Array D DIM2 RGB
gradient r c = fromFunction (Z :. r :. c) getPx where
  (r', c') = (fromIntegral r, fromIntegral c)
  getPx (Z :. i :. j) = RGB (i'/r') (j'/c') ((i' + j') / (r'+c')) where
    (i', j') = (fromIntegral i, fromIntegral j)

gradient' :: Int -> Int -> Array D DIM2 RGB
gradient' r c = fromFunction (Z :. r :. c) getPx where
  (r', c') = (fromIntegral r, fromIntegral c)
  getPx (Z :. (fromIntegral -> i) :. (fromIntegral -> j)) =
    RGB (i/r') (j/c') ((i + j) / (r'+c'))


gradientV :: Int -> Int -> V.Vector Gray
gradientV r c = V.generate (r*c) (\i -> Gray ((fromIntegral i) / (r'*c'))) where
  (r', c') = (fromIntegral r, fromIntegral c)


reshape' :: Array D DIM2 RGB -> Array D DIM2 RGB
reshape' arr@(extent -> (Z :. r :. c)) = reshape (Z :. r*2 :. c `div` 2) arr


multS :: Array U DIM2 RGB -> Array U DIM2 RGB -> Array U DIM2 RGB
multS arr1@(extent -> (Z :. m1 :. _)) arr2@(extent -> (Z :. _ :. n2)) =
  computeS $ fromFunction (Z :. m1 :. n2) getPx where
    getPx (Z:. i :. j) = sumAllS $ zipWith (*)
                         (slice arr1 (Any :. (i :: Int) :. All))
                         (slice arr2 (Any :. (j :: Int)))


-- | WRONG: nested parallelism.
multP :: Array U DIM2 RGB -> Array U DIM2 RGB -> Array U DIM2 RGB
multP arr1@(extent -> (Z :. m1 :. _)) arr2@(extent -> (Z :. _ :. n2)) =
  head $ computeP $ fromFunction (Z :. m1 :. n2) getPx where
    getPx (Z:. i :. j) = head $ sumAllP $ zipWith (*)
                         (slice arr1 (Any :. (i :: Int) :. All))
                         (slice arr2 (Any :. (j :: Int)))


-- | CORRECT: no nested parallelism, since sum is sequential
multP' :: Array U DIM2 RGB -> Array U DIM2 RGB -> Array U DIM2 RGB
multP' arr1@(extent -> (Z :. m1 :. _)) arr2@(extent -> (Z :. _ :. n2)) =
  head $ computeP $ fromFunction (Z :. m1 :. n2) getPx where
    getPx (Z:. i :. j) = sumAllS $ zipWith (*)
                         (slice arr1 (Any :. (i :: Int) :. All))
                         (slice arr2 (Any :. (j :: Int)))



displayA :: (I.Saveable I.Image px, Pixel px) => Array D DIM2 px -> IO ()
displayA arr = do
  arr' <- computeP arr
  I.displayImage $ I.fromArray arr'
                                         
main :: IO ()  
main = do
  frog <- I.readImageRGB "images/downloaded/frog-640x412.jpg"
  centaurus <- I.readImageRGB "images/downloaded/centaurus-galaxy.jpg" -- 400x640
  cluster <- I.readImageRGB "images/downloaded/star-cluster.jpg" -- 624x640

  let !frogA = I.toArray frog
  --let !centaurusA = I.toArray centaurus
  --let !clusterA = I.toArray cluster
  --I.writeImage "frog_mod.jpg" frog []
  
  -- | Display gradient sequential  
  --I.displayImage $ I.fromArray $ computeS $ gradient 300 400
  
  -- | Display gradient parallel  
  --I.displayImage $ I.fromArray $ head $ computeP $ gradient' 300 400

  -- | Display Gray Gradient creted from a Vector (have to delay, since it's unboxed)
  --displayA $ delay $ fromUnboxed (Z :. 200 :. 300) $ gradientV 200 300
  
  --let g = gradient 300 400
  -- | reshape an array
  --displayA $ reshape' g

  -- | concat two arrays
  --displayA $ g ++ g

  -- | extract a sub array
  -- Possible warning for nested parallelism, use BangPatterns
  --displayA $ extract (Z :. 200 :. 100) (Z :. 200 :. 300) frogA

  -- | transpose an array  
  --displayA $ transpose frogA

  -- | extract + zipWith + map
  --let clusterA' = extract (Z :. 0 :. 0) (Z :. 400 :. 640) clusterA
  -- | even better:
  --let clusterA' = extract (Z :. 0 :. 0) (extent centaurusA) clusterA
  --displayA $ map (/2) $ zipWith (+) clusterA' centaurusA
  -- | alternative:
  -- displayA $ map (/2) $ clusterA' +^ centaurusA
  -- | alternative using traverse2
  --displayA $ traverse2 centaurusA clusterA
  --  (\sh _ -> sh)
  --  (\getPx1 getPx2 idx -> (getPx1 idx + getPx2 idx) / 2)
  --I.writeImage "mix.jpg" (I.fromArray $ computeS mix) []

  -- | Expensive computation:
  -- | Sequential
  -- let !frogAt = computeS $ transpose frogA
  -- I.writeImage "mult.jpg" (I.fromArray $ multS frogA frogAt) []

  -- | Parallel
  let !frogAt = head $ computeP $ transpose frogA
  I.writeImage "mult_p.jpg" (I.fromArray $ multP' frogA frogAt) []
  return ()

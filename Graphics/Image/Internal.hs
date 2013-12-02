{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, ViewPatterns #-}

module Graphics.Image.Internal where

import Prelude hiding ((++))
import qualified Prelude as P (floor)
import Data.Array.Repa.Eval
import qualified Data.Vector.Unboxed as V


class (Elt px, V.Unbox px, Floating px, Fractional px, Num px, Eq px, Show px) =>
      Pixel px where
  pixel :: Double -> px
       
  pxOp :: (Double -> Double) -> px -> px

  pxOp2 :: (Double -> Double -> Double) -> px -> px -> px

  strongest :: px -> px

  weakest :: px -> px


class Convertable px1 px2 where
  convert :: px1 -> px2


class Pixel px => Processable img px | px -> img where

  -- | Get a pixel at m-th row and n-th column
  ref :: Pixel px => img px -> Int -> Int -> px

  -- | Make an Image by supplying number of rows, columns and a function that
  -- returns a pixel value at the m n location which are provided as arguments.
  make :: Pixel px => Int -> Int -> (Int -> Int -> px) -> img px

  {-| Map a function over an image with a function. -}
  map :: (Pixel px, Pixel px1) => (px -> px1) -> img px -> img px1

  -- | Zip two Images with a function. Images do not have to hold the same type
  -- of pixels.
  zipWith :: (Pixel px, Pixel px2, Pixel px3) =>
                  (px -> px2 -> px3) -> img px -> img px2 -> img px3

  -- | Fold an Image.
  fold :: Pixel px => (px -> px -> px) -> px -> img px -> px

  -- | Traverse the image.
  traverse :: Pixel px =>
              img px ->
              (Int -> Int -> (Int, Int)) ->
              ((Int -> Int -> px) -> Int -> Int -> px1) ->
              img px1

  -- | O(1) Convert an Unboxed Vector to an Image by supplying rows, columns and
  -- a vector
  fromVector :: Pixel px => Int -> Int -> V.Vector px -> img px

  -- | O(1) Convert an Image to a Vector of length: rows*cols
  toVector :: Pixel px => img px -> V.Vector px

  -- | Get dimensions of the image. (rows, cols)
  dims :: Pixel px => img px -> (Int, Int)

  -- | Get the number of rows in the image 
  rows :: Pixel px => img px -> Int
  {-# INLINE rows #-}
  rows = fst . dims

  -- | Get the number of columns in the image
  cols :: Pixel px => img px -> Int
  {-# INLINE cols #-}
  cols = snd . dims
  
  -- | Get a pixel at i j location with a default pixel. If i or j are greater
  -- then number rows or columns respectively, default pixel will be used
  refd :: Pixel px => img px -> px -> Int -> Int -> px
  {-# INLINE refd #-}
  refd img def i j = maybe def id $ refm img i j
  
  
  -- | Get Maybe pixel at i j location. If i or j are greater then number of rows or
  -- columns respectively will return Nothing
  refm :: Pixel px => img px -> Int -> Int -> Maybe px
  {-# INLINE refm #-}
  refm img@(dims -> (m, n)) i j
     | i >= 0 && j >= 0 && i < m && j < n = Just $ ref img i j
     | otherwise = Nothing
  
  -- | Convert an Image to a nested List of Pixels.
  toLists :: Pixel px => img px -> [[px]]
  {-# INLINE toLists #-}
  toLists img =
    [[ref img m n | n <- [0..cols img - 1]] | m <- [0..rows img - 1]]
  
  -- | Convert a nested List of Pixels to an Image.
  fromLists :: Pixel px => [[px]] -> img px
  {-# INLINE fromLists #-}
  fromLists ls =
    (fromVector (length ls) (length $ head ls)) . V.fromList . concat $ ls

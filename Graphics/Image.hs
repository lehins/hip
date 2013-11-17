{-# LANGUAGE ViewPatterns #-}
module Graphics.Image where

import Prelude hiding (map, zipWith)
import Graphics.Image.Gray
import Graphics.Image.Color
import Graphics.Image.Internal
import Graphics.Image.Algorithms
import qualified Data.Vector.Unboxed as V

dim img = (width img, height img)

toList img =
  [[ref img x y | x <- [0..width img]] | y <- [0..height img]]

fromList ls =
  (fromVector (length $ head ls) (length ls)) . V.fromList . concat $ ls

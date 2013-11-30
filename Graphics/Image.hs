{-# LANGUAGE ViewPatterns #-}
module Graphics.Image where

import Prelude hiding (map, zipWith)
import Graphics.Image.Gray
import Graphics.Image.Color
import Graphics.Image.Base
import Graphics.Image.Algorithms
import qualified Data.Vector.Unboxed as V

toLists img =
  [[ref img m n | n <- [0..cols img - 1]] | m <- [0..rows img - 1]]

fromLists ls =
  (fromVector (length ls) (length $ head ls)) . V.fromList . concat $ ls


module Graphics.Image.Algorithms where

import Prelude hiding (map, fold, zipWith)
import Graphics.Image.Base (Pixel(..))
import Graphics.Image.Internal

getLargest :: (Ord px, Pixel px) => Image px -> px
getLargest img = fold max (ref img 0 0) img

getSmallest :: (Ord px, Pixel px) => Image px -> px
getSmallest img = fold min (ref img 0 0) img

normalize :: (Ord px, Pixel px) => Image px -> Image px
normalize img
  | s == w = img * 0
  | otherwise = map normalizer img
  where (s, w) = (strongest (getLargest img), weakest (getSmallest img))
        normalizer px = (px - w)/(s - w)


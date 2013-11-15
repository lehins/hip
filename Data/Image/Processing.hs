
module Data.Image.Processing where

import Data.Image.Internal

getLargest :: (Ord px, Pixel px) => Image px -> px
getLargest img = imageFold max (ref img 0 0) img

getSmallest :: (Ord px, Pixel px) => Image px -> px
getSmallest img = imageFold min (ref img 0 0) img

normalize :: (Ord px, Pixel px) => Image px -> Image px
normalize img
  | s == w = img * 0
  | otherwise = imageMap normalizer img
  where (s, w) = (strongest (getLargest img), weakest (getSmallest img))
        normalizer px = (px - w)/(s - w)


{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Graphics.Image.Interface.Repa
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Repa (
  -- * Construction
  makeImageS, makeImageP, fromListsS, fromListsP,
  -- * Representation
  RS(..), RP(..),
  -- * Conversion
  fromRepaArrayS, fromRepaArrayP, toRepaArray
  ) where

import Graphics.Image.Interface hiding (makeImage, fromLists)
import qualified Graphics.Image.Interface as I (makeImage, fromLists)
import Graphics.Image.Interface.Repa.Internal


-- | Create an image with sequential array representation.
makeImageS :: Array RS cs Double =>
             (Int, Int) -- ^ (@m@ rows, @n@ columns) - dimensions of a new image.
          -> ((Int, Int) -> Pixel cs Double)
             -- ^ A function that takes (@i@-th row, and @j@-th column) as an argument
             -- and returns a pixel for that location.
          -> Image RS cs Double
makeImageS = I.makeImage
{-# INLINE makeImageS #-}

-- | Create an image with parallel array representation.
makeImageP :: Array RP cs Double =>
             (Int, Int) -- ^ (@m@ rows, @n@ columns) - dimensions of a new image.
          -> ((Int, Int) -> Pixel cs Double)
             -- ^ A function that takes (@i@-th row, and @j@-th column) as an argument
             -- and returns a pixel for that location.
          -> Image RP cs Double
makeImageP = I.makeImage
{-# INLINE makeImageP #-}


-- | Construct an image from a nested rectangular shaped list of pixels sequentially.
fromListsS
  :: Array RS cs e
  => [[Pixel cs e]] -> Image RS cs e
fromListsS = I.fromLists
{-# INLINE fromListsS #-}

-- | Construct an image from a nested rectangular shaped list of pixels in parallel.
fromListsP
  :: Array RP cs e
  => [[Pixel cs e]] -> Image RP cs e
fromListsP = I.fromLists
{-# INLINE fromListsP #-}

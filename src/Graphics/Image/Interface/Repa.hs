{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}
-- |
-- Module      : Graphics.Image.Interface.Repa
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Repa
  -- * Conversion
  ( fromRepaArrayS
  , fromRepaArrayP
  , fromRepaArray
  , toRepaArray
  , toRepaArrayD
  -- * Representation
  , RSU
  , RPU
  , RSS
  , RPS
  , Repr(..)
  ) where

import qualified Data.Array.Repa                        as R
import qualified Data.Array.Repa.Eval                   as R
import           Data.Array.Repa.Index
import           Graphics.Image.Interface
import           Graphics.Image.Interface.Repa.Generic
import           Graphics.Image.Interface.Repa.Storable
import           Graphics.Image.Interface.Repa.Unboxed
import           Graphics.Image.Internal



-- | Create a sequential unboxed image from a 2D Repa delayed array.
fromRepaArrayS :: R.Source r (Pixel cs e) => R.Array r DIM2 (Pixel cs e) -> Image RSU cs e
fromRepaArrayS = Image . RSUArray . fromRepaArrayR


-- | Create a parallel unboxed image from a 2D Repa delayed array.
fromRepaArrayP :: R.Source r (Pixel cs e) => R.Array r DIM2 (Pixel cs e) -> Image RPU cs e
fromRepaArrayP = Image . RPUArray . fromRepaArrayR


-- | Create am Image from Repa array.
fromRepaArray :: (Array arr cs e, R.Source r (Pixel cs e)) =>
                 Repr arr -> R.Array r DIM2 (Pixel cs e) -> Image arr cs e
fromRepaArray _ arr = makeImage (sh2ix (R.extent arr)) (R.index arr . ix2sh)


-- | Convert an image into Repa Unboxed array.
toRepaArray :: (R.Elt e, Array arr cs e) => Image arr cs e -> R.Array R.U DIM2 (Pixel cs e)
toRepaArray (exchange RSU -> (Image (RSUArray arr))) = toRepaArrayR arr


-- | Convert an image into Repa Delayed array.
toRepaArrayD :: Array arr cs e => Image arr cs e -> R.Array R.D DIM2 (Pixel cs e)
toRepaArrayD img =
  let Image arr = compute img
  in R.fromFunction (ix2sh (shapeA arr)) (unsafeIndexA arr . sh2ix)

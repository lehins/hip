{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Graphics.Image.Interface.Repa
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Repa (
  -- * Conversion
  fromRepaArrayS, fromRepaArrayP,
  toRepaArray,
  -- * Representation
  RSU(..), RPU(..), RSS(..), RPS(..)
  ) where

import Data.Array.Repa.Index
import qualified Data.Array.Repa as R
import qualified Data.Vector.Generic as VG

import Graphics.Image.Interface
import Graphics.Image.Interface.Repa.Generic
import Graphics.Image.Interface.Repa.Storable
import Graphics.Image.Interface.Repa.Unboxed



-- | Create a sequential unboxed image from a 2D Repa delayed array.
fromRepaArrayS :: R.Source r (Pixel cs e) => R.Array r DIM2 (Pixel cs e) -> Image RSU cs e
fromRepaArrayS = SUImage . fromRepaArrayR


-- | Create a parallel unboxed image from a 2D Repa delayed array.
fromRepaArrayP :: R.Source r (Pixel cs e) => R.Array r DIM2 (Pixel cs e) -> Image RPU cs e
fromRepaArrayP = PUImage . fromRepaArrayR


-- | Convert into Repa Unboxed array from an image.
toRepaArray :: Array arr cs e => Image arr cs e -> R.Array R.U DIM2 (Pixel cs e)
toRepaArray img = R.fromUnboxed (ix2sh (dims img)) $ VG.convert $ toVector img

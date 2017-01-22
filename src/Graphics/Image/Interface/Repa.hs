{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  --fromRepaArrayS, fromRepaArrayP,
  toRepaArray,
  -- * Representation
  RSU(..), RPU(..), RSS(..), RPS(..)
  ) where

import Data.Array.Repa.Index
import qualified Data.Array.Repa as R

import Graphics.Image.Interface
import Graphics.Image.Interface.Repa.Generic
import Graphics.Image.Interface.Repa.Storable
import Graphics.Image.Interface.Repa.Unboxed
import Graphics.Image.Interface.Vector


-- | Makes a copy of an image into a Storable Vector representation.
instance Exchangable VU RSS where
  exchange = exchangeThrough VS
  {-# INLINE exchange #-}


-- | Makes a copy of an image into a Storable Vector representation.
instance Exchangable VU RPS where
  exchange = exchangeThrough VS
  {-# INLINE exchange #-}



-- | Makes a copy of an image into a Storable Vector representation.
instance Exchangable VS RSU where
  exchange = exchangeThrough VU
  {-# INLINE exchange #-}


-- | Makes a copy of an image into a Storable Vector representation.
instance Exchangable VS RPU where
  exchange = exchangeThrough VU
  {-# INLINE exchange #-}


-- | Makes a copy of an image into a Storable Vector representation.
instance Exchangable RSU VS where
  exchange _ = exchange VS . toManifest
  {-# INLINE exchange #-}


-- | Makes a copy of an image into a Storable Vector representation.
instance Exchangable RPU VS where
  exchange _ = exchange VS . toManifest
  {-# INLINE exchange #-}



-- | Makes a copy of an image into a Unboxed Vector representation.
instance Exchangable RSS VU where
  exchange _ = exchange VU . toManifest
  {-# INLINE exchange #-}


-- | Makes a copy of an image into a Unboxed Vector representation.
instance Exchangable RPS VU where
  exchange _ = exchange VU . toManifest
  {-# INLINE exchange #-}


-- | Makes a copy of an image into a Storable representation sequentially.
instance Exchangable RSU RSS where
  exchange _ (SUImage (SScalar px)) = SSImage (SScalar px)
  exchange _ (SUImage img)          = SSImage . compute . SDImage . getDelayedS $ img
  {-# INLINE exchange #-}


-- | Makes a copy of an image into a Storable representation sequentially.
instance Exchangable RPU RPS where
  exchange _ (PUImage (PScalar px)) = PSImage (PScalar px)
  exchange _ (PUImage img)          = PSImage . compute . PDImage . getDelayedP $ img
  {-# INLINE exchange #-}


-- | Makes a copy of an image into a Unboxed representation sequentially.
instance Exchangable RSS RSU where
  exchange _ (SSImage (SScalar px)) = SUImage (SScalar px)
  exchange _ (SSImage img)          = SUImage . compute . SDImage . getDelayedS $ img
  {-# INLINE exchange #-}


-- | Makes a copy of an image into a Unboxed representation sequentially.
instance Exchangable RPS RPU where
  exchange _ (PSImage (PScalar px)) = PUImage (PScalar px)
  exchange _ (PSImage img)          = PUImage . compute . PDImage . getDelayedP $ img
  {-# INLINE exchange #-}



-- -- | Create a sequential image from a 2D Repa delayed array.
-- fromRepaArrayS :: R.Array R.D DIM2 (Pixel cs e) -> Image RS cs e
-- fromRepaArrayS = SDImage


-- -- | Create a parallel image from a 2D Repa delayed array.
-- fromRepaArrayP :: R.Array R.D DIM2 (Pixel cs e) -> Image RP cs e
-- fromRepaArrayP = PDImage

-- | Retrieve an underlying Repa array from an image.
toRepaArray
  :: (Array arr cs e, Array RSU cs e, Exchangable arr RSU)
  => Image arr cs e -> R.Array R.U DIM2 (Pixel cs e)
toRepaArray img =
  case exchange RSU img of
    (SUImage (STImage arr)) -> arr
    (SUImage (SDImage arr)) -> R.computeS arr
    (SUImage (SScalar px))  -> R.computeS $ R.fromFunction (Z :. 1 :. 1) $ const px

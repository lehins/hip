{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Graphics.Image.Interface.Vector
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Vector (
  -- * Representation
  VU(..), VS(..),
  -- * Conversion
  fromUnboxedVector, toUnboxedVector,
  fromStorableVector, toStorableVector,
  -- * Linear index conversion
  toIx, fromIx
  ) where

import Data.Vector as V (convert)
import Graphics.Image.Interface
import Graphics.Image.Interface.Vector.Generic
import Graphics.Image.Interface.Vector.Unboxed
import Graphics.Image.Interface.Vector.Storable


instance Exchangable VU VS where
  exchange _ (VUImage (VScalar px))   = VSImage (VScalar px)
  exchange _ (VUImage (VImage m n v)) = VSImage (VImage m n (V.convert v))


instance Exchangable VS VU where
  exchange _ (VSImage (VScalar px))   = VUImage (VScalar px)
  exchange _ (VSImage (VImage m n v)) = VUImage (VImage m n (V.convert v))

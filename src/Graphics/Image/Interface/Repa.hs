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
  -- * Conversion
  --fromRepaArrayS, fromRepaArrayP, toRepaArray,
  -- * Representation
  RSU(..), RPU(..),
  ) where

import Graphics.Image.Interface.Repa.Unboxed




-- -- | Changes to Sparse Vector representation.
-- instance Exchangable RS VS where
--   exchange arr = exchange arr . toManifest
--   {-# INLINE exchange #-}


-- -- | Changes to Sparse Vector representation.
-- instance Exchangable RP VS where
--   exchange arr = exchange arr . toManifest
--   {-# INLINE exchange #-}


-- -- | Changes from Sparse Vector representation.
-- instance Exchangable VS RS where
--   exchange arr = exchange arr . toManifest
--   {-# INLINE exchange #-}


-- -- | Changes from Sparse Vector representation.
-- instance Exchangable VS RP where
--   exchange arr = exchange arr . toManifest
--   {-# INLINE exchange #-}


-- -- | Create a sequential image from a 2D Repa delayed array.
-- fromRepaArrayS :: R.Array R.D DIM2 (Pixel cs e) -> Image RS cs e
-- fromRepaArrayS = SDImage


-- -- | Create a parallel image from a 2D Repa delayed array.
-- fromRepaArrayP :: R.Array R.D DIM2 (Pixel cs e) -> Image RP cs e
-- fromRepaArrayP = PDImage


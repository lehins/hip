{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
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
  -- * Filtering
  filter, ifilter,
  -- * Linear index conversion
  toIx, fromIx
  ) where

import Prelude hiding (filter)
import qualified Data.Vector.Unboxed as U
import Graphics.Image.Interface as I
import Graphics.Image.Interface.Vector.Unboxed
import Graphics.Image.Interface.Vector.Storable


-- | Filter out Pixels from an image that do not satisfy the predicate and
-- convert a result into a flat unboxed vector with indexed Pixels.
filter :: Array arr cs e =>
          (Pixel cs e -> Bool) -- ^ The predicate
       -> Image arr cs e -- ^ Source image
       -> U.Vector ((Int, Int), Pixel cs e)
filter f !img = U.filter (f . snd) $ U.imap addIx $ U.convert $ toVector img where
  (_, n) = dims img
  addIx !k !px = (toIx n k, px)


-- | Filter out Pixels from an image that do not satisfy the index aware
-- predicate and convert a result into a flat unboxed vector with indexed
-- Pixels.
ifilter :: Array arr cs e =>
           ((Int, Int) -> Pixel cs e -> Bool) -- ^ The predicate
        -> Image arr cs e -- ^ Source image
        -> U.Vector ((Int, Int), Pixel cs e)
ifilter f !img = U.filter (uncurry f) $ U.imap addIx $ U.convert $ toVector img where
  (_, n) = dims img
  addIx !k !px = (toIx n k, px)


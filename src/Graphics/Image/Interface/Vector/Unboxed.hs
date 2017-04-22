{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Graphics.Image.Interface.Vector.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Vector.Unboxed
  ( VU(..)
  , Repr(VU)
  , VU.Unbox
  ) where

import           Control.DeepSeq                          (NFData(..), deepseq)
import           Prelude                                  hiding (map, zipWith)
-- import           Data.Typeable                            (Typeable)
import qualified Data.Vector.Generic                      as VG
import qualified Data.Vector.Unboxed                      as VU
import           Graphics.Image.Interface                 as I
import           Graphics.Image.Interface.Vector.Generic
import           Graphics.Image.Interface.Vector.Unboxing ()



-- | Unboxed 'Vector' representation.
newtype VU ix px = VUArray (VGArray VU.Vector px)

data instance Repr VU = VU


instance NFData px => NFData (VU (Int, Int) px) where
  rnf (VUArray arr) = rnf arr

type instance Vector VU = VU.Vector



instance VU.Unbox e => BaseArray VU (Int, Int) e where

  type SuperClass VU (Int, Int) e = (VG.Vector VU.Vector e, VU.Unbox e)

  shapeA (VUArray v) = dimsVG v
  {-# INLINE shapeA #-}

  makeA !sz = VUArray . makeArrayVG sz
  {-# INLINE makeA #-}

  unsafeIndexA (VUArray arr) = unsafeIndexVG arr
  {-# INLINE unsafeIndexA #-}


instance BaseArray VU (Int, Int) e => IArray VU (Int, Int) e where

  makeWindowedA !sh !wIx !wSz f g = VUArray $ makeArrayWindowedVG sh wIx wSz f g
  {-# INLINE makeWindowedA #-}

  scalarA = VUArray . scalarVG
  {-# INLINE scalarA #-}

  mapA f (VUArray v) = VUArray $ mapVG f v
  {-# INLINE mapA #-}

  imapA f (VUArray v) = VUArray $ imapVG f v
  {-# INLINE imapA #-}

  zipWithA f (VUArray v1) (VUArray v2) = VUArray $ zipWithVG f v1 v2
  {-# INLINE zipWithA #-}

  izipWithA f (VUArray v1) (VUArray v2) = VUArray $ izipWithVG f v1 v2
  {-# INLINE izipWithA #-}

  traverseA (VUArray v) f g = VUArray $ traverseVG v f g
  {-# INLINE traverseA #-}

  traverse2A (VUArray v1) (VUArray v2) f g = VUArray $ traverse2VG v1 v2 f g
  {-# INLINE traverse2A #-}

  transposeA (VUArray v) = VUArray $ transposeVG v
  {-# INLINE transposeA #-}

  backpermuteA !sz f (VUArray v) = VUArray $ backpermuteVG sz f v
  {-# INLINE backpermuteA #-}

  fromListsA = VUArray . fromListsVG
  {-# NOINLINE fromListsA #-}

  foldA f !px0 (VUArray v) = foldlVG f px0 v
  {-# INLINE foldA #-}

  foldIxA f !px0 (VUArray v) = ifoldlVG f px0 v
  {-# INLINE foldIxA #-}

  multA (VUArray v1) (VUArray v2) = VUArray (multVG v1 v2)
  {-# INLINE multA #-}

  computeA (VUArray v) = v `deepseq` VUArray v
  {-# INLINE computeA #-}


instance BaseArray VU (Int, Int) e => ManifestArray VU (Int, Int) e where

  foldlA f !px0 (VUArray arr) = foldlVG f px0 arr
  {-# INLINE foldlA #-}

  foldrA f !px0 (VUArray arr) = foldrVG f px0 arr
  {-# INLINE foldrA #-}

  makeArrayMA !sh f = VUArray <$> makeArrayMVG sh f
  {-# INLINE makeArrayMA #-}

  mapMA f (VUArray arr) = VUArray <$> mapMVG f arr
  {-# INLINE mapMA #-}

  mapM_A f (VUArray arr) = mapM_VG f arr
  {-# INLINE mapM_A #-}

  foldMA f !px0 (VUArray arr) = foldMVG f px0 arr
  {-# INLINE foldMA #-}

  foldM_A f !px0 (VUArray arr) = foldM_VG f px0 arr
  {-# INLINE foldM_A #-}


-- data MVU s ix px = MVUArray !(MVGArray s VU.Vector px)

-- type instance MA VU = MVU

-- instance VU.Unbox e => MutableArray MVU (Int, Int) e where

--   thawA (VUArray arr) = MVUArray <$> thawVG arr
--   {-# INLINE thawA #-}

--   freezeA (MVUArray arr) = VUArray <$> freezeVG arr
--   {-# INLINE freezeA #-}

--   mshapeA (MVUArray marr) = mdimsVG marr
--   {-# INLINE mshapeA #-}

--   newA !ix = MVUArray <$> newVG ix
--   {-# INLINE newA #-}

--   readA (MVUArray arr) = readVG arr
--   {-# INLINE readA #-}

--   writeA (MVUArray arr) = writeVG arr
--   {-# INLINE writeA #-}

--   swapA (MVUArray arr) = swapVG arr
--   {-# INLINE swapA #-}


instance ColorSpace cs e => Eq (VU (Int,Int) (Pixel cs e)) where
  (VUArray arr1) == (VUArray arr2) = arr1 == arr2


instance ColorSpace cs e => Array VU cs e where

  toVectorA (VUArray v) = toVectorVG v
  {-# INLINE toVectorA #-}

  fromVectorA !sz = VUArray . fromVectorVG sz
  {-# INLINE fromVectorA #-}


instance ColorSpace cs e => MArray VU cs e

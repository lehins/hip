{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Graphics.Image.Interface.Vector.Storable
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Vector.Storable
  ( VS(..)
  , Repr(VS)
  ) where

import           Control.DeepSeq                          (NFData (..), deepseq)
import           Prelude                                  hiding (map, zipWith)
-- import           Data.Typeable                            (Typeable)
import qualified Data.Vector.Storable                     as VS
import qualified Data.Vector.Generic                      as VG
import           Graphics.Image.Interface                 as I
import           Graphics.Image.Interface.Vector.Generic



-- | Unboxed 'Vector' representation.
newtype VS ix px = VSArray (VGArray VS.Vector px)

data instance Repr VS = VS


instance NFData px => NFData (VS (Int, Int) px) where
  rnf (VSArray arr) = rnf arr

type instance Vector VS = VS.Vector



instance VS.Storable e => BaseArray VS (Int, Int) e where

  type SuperClass VS (Int, Int) e = (VG.Vector VS.Vector e, VS.Storable e)

  unsafeIndexA (VSArray arr) = unsafeIndexVG arr
  {-# INLINE unsafeIndexA #-}

  shapeA (VSArray v) = dimsVG v
  {-# INLINE shapeA #-}

  makeA !sz = VSArray . makeImageVG sz
  {-# INLINE makeA #-}


instance BaseArray VS (Int, Int) e => IArray VS (Int, Int) e where

  makeWindowedA !sh !wIx !wSz f g = VSArray $ makeImageWindowedVG sh wIx wSz f g
  {-# INLINE makeWindowedA #-}

  scalarA = VSArray . scalarVG
  {-# INLINE scalarA #-}

  mapA f (VSArray v) = VSArray $ mapVG f v
  {-# INLINE mapA #-}

  imapA f (VSArray v) = VSArray $ imapVG f v
  {-# INLINE imapA #-}

  zipWithA f (VSArray v1) (VSArray v2) = VSArray $ zipWithVG f v1 v2
  {-# INLINE zipWithA #-}

  izipWithA f (VSArray v1) (VSArray v2) = VSArray $ izipWithVG f v1 v2
  {-# INLINE izipWithA #-}

  traverseA (VSArray v) f g = VSArray $ traverseVG v f g
  {-# INLINE traverseA #-}

  traverse2A (VSArray v1) (VSArray v2) f g = VSArray $ traverse2VG v1 v2 f g
  {-# INLINE traverse2A #-}

  transposeA (VSArray v) = VSArray $ transposeVG v
  {-# INLINE transposeA #-}

  backpermuteA !sz f (VSArray v) = VSArray $ backpermuteVG sz f v
  {-# INLINE backpermuteA #-}

  fromListsA = VSArray . fromListsVG
  {-# NOINLINE fromListsA #-}

  foldA f !px0 (VSArray v) = foldlVG f px0 v
  {-# INLINE foldA #-}

  foldIxA f !px0 (VSArray v) = ifoldlVG f px0 v
  {-# INLINE foldIxA #-}

  multA (VSArray v1) (VSArray v2) = VSArray (multVG v1 v2)
  {-# INLINE multA #-}

  computeA (VSArray v) = v `deepseq` VSArray v
  {-# INLINE computeA #-}

instance BaseArray VS (Int, Int) e => ManifestArray VS (Int, Int) e where

  foldlA f !px0 (VSArray arr) = foldlVG f px0 arr
  {-# INLINE foldlA #-}

  foldrA f !px0 (VSArray arr) = foldrVG f px0 arr
  {-# INLINE foldrA #-}

  makeArrayMA !sh f = VSArray <$> makeImageMVG sh f
  {-# INLINE makeArrayMA #-}

  mapMA f (VSArray arr) = VSArray <$> mapMVG f arr
  {-# INLINE mapMA #-}

  mapM_A f (VSArray arr) = mapM_VG f arr
  {-# INLINE mapM_A #-}

  foldMA f !px0 (VSArray arr) = foldMVG f px0 arr
  {-# INLINE foldMA #-}

  foldM_A f !px0 (VSArray arr) = foldM_VG f px0 arr
  {-# INLINE foldM_A #-}


-- data MVS s ix px = MVSArray !(MVGArray s VS.Vector px)

-- type instance MA VS = MVS

-- instance VS.Unbox e => MutableArray MVS (Int, Int) e where

--   thawA (VSArray arr) = MVSArray <$> thawVG arr
--   {-# INLINE thawA #-}

--   freezeA (MVSArray arr) = VSArray <$> freezeVG arr
--   {-# INLINE freezeA #-}

--   mshapeA (MVSArray marr) = mdimsVG marr
--   {-# INLINE mshapeA #-}

--   newA !ix = MVSArray <$> newVG ix
--   {-# INLINE newA #-}

--   readA (MVSArray arr) = readVG arr
--   {-# INLINE readA #-}

--   writeA (MVSArray arr) = writeVG arr
--   {-# INLINE writeA #-}

--   swapA (MVSArray arr) = swapVG arr
--   {-# INLINE swapA #-}



instance (VS.Storable (Pixel cs e), ColorSpace cs e) => Eq (VS (Int,Int) (Pixel cs e)) where
  (VSArray arr1) == (VSArray arr2) = arr1 == arr2


instance (VS.Storable (Pixel cs e), ColorSpace cs e) => Array VS cs e where

  toVectorA (VSArray v) = toVectorVG v
  {-# INLINE toVectorA #-}

  fromVectorA !sz = VSArray . fromVectorVG sz
  {-# INLINE fromVectorA #-}


instance (VS.Storable (Pixel cs e), ColorSpace cs e) => MArray VS cs e

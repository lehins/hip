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
module Graphics.Image.Interface.Massiv
  ( PU(..)
  , I.Repr(PU)
  ) where

-- import           Control.DeepSeq                          (NFData(..), deepseq)
import           Prelude                                  hiding (map, zipWith)
-- import           Data.Typeable                            (Typeable)
-- -- import qualified Data.Vector.Generic                      as VG
import qualified Data.Vector.Unboxed                      as VU
import qualified Graphics.Image.Interface                 as I
-- import           Graphics.Image.Interface.Vector.Generic
import           Graphics.Image.Interface.Vector.Unboxing ()

import Data.Array.Massiv


import Data.Array.Massiv.Windowed





-- | Unboxed 'Vector' representation.
data PU ix px
  = PDArray !(Array D DIM2 px)
  | PMArray !(Array M DIM2 px)

data instance I.Repr PU = PU


type instance I.Vector PU = VU.Vector



instance VU.Unbox e => I.BaseArray PU (Int, Int) e where

  type SuperClass PU (Int, Int) e = (VU.Unbox e)

  shapeA (PDArray arr) = size arr
  shapeA (PMArray arr) = size arr
  {-# INLINE shapeA #-}

  makeA !sz = PDArray . makeArray2D sz
  {-# INLINE makeA #-}

  --unsafeIndexA (PDArray arr) = unsafeIndex arr
  unsafeIndexA (PMArray arr) = unsafeIndex arr
  {-# INLINE unsafeIndexA #-}


instance I.BaseArray PU (Int, Int) e => I.IArray PU (Int, Int) e where

  makeWindowedA !sz !wIx !wSz wGetPx bGetPx =
    PMArray $ computeUnboxedS $ makeArrayWindowed (makeArray2D sz bGetPx) wIx wSz wGetPx
  {-# INLINE makeWindowedA #-}

  scalarA = PMArray . MArray (1,1) . const
  {-# INLINE scalarA #-}

--   mapA f (PUArray v) = PUArray $ mapVG f v
--   {-# INLINE mapA #-}

--   imapA f (PUArray v) = PUArray $ imapVG f v
--   {-# INLINE imapA #-}

--   zipWithA f (PUArray v1) (PUArray v2) = PUArray $ zipWithVG f v1 v2
--   {-# INLINE zipWithA #-}

--   izipWithA f (PUArray v1) (PUArray v2) = PUArray $ izipWithVG f v1 v2
--   {-# INLINE izipWithA #-}

--   traverseA (PUArray v) f g = PUArray $ traverseVG v f g
--   {-# INLINE traverseA #-}

--   traverse2A (PUArray v1) (PUArray v2) f g = PUArray $ traverse2VG v1 v2 f g
--   {-# INLINE traverse2A #-}

--   transposeA (PUArray v) = PUArray $ transposeVG v
--   {-# INLINE transposeA #-}

--   backpermuteA !sz f (PUArray v) = PUArray $ backpermuteVG sz f v
--   {-# INLINE backpermuteA #-}

--   fromListsA = PUArray . fromListsVG
--   {-# NOINLINE fromListsA #-}

--   foldA f !px0 (PUArray v) = foldlVG f px0 v
--   {-# INLINE foldA #-}

--   foldIxA f !px0 (PUArray v) = ifoldlVG f px0 v
--   {-# INLINE foldIxA #-}

--   multA (PUArray v1) (PUArray v2) = PUArray (multVG v1 v2)
--   {-# INLINE multA #-}

  computeA (PDArray arr) = PMArray (computeUnboxedS arr)
  computeA iarr@(PMArray _) = iarr
  {-# INLINE computeA #-}


-- instance BaseArray PU (Int, Int) e => ManifestArray PU (Int, Int) e where

--   foldlA f !px0 (PUArray arr) = foldlVG f px0 arr
--   {-# INLINE foldlA #-}

--   foldrA f !px0 (PUArray arr) = foldrVG f px0 arr
--   {-# INLINE foldrA #-}

--   makeArrayMA !sh f = PUArray <$> makeArrayMVG sh f
--   {-# INLINE makeArrayMA #-}

--   mapMA f (PUArray arr) = PUArray <$> mapMVG f arr
--   {-# INLINE mapMA #-}

--   mapM_A f (PUArray arr) = mapM_VG f arr
--   {-# INLINE mapM_A #-}

--   foldMA f !px0 (PUArray arr) = foldMVG f px0 arr
--   {-# INLINE foldMA #-}

--   foldM_A f !px0 (PUArray arr) = foldM_VG f px0 arr
--   {-# INLINE foldM_A #-}


-- -- data MPU s ix px = MPUArray !(MVGArray s PU.Vector px)

-- -- type instance MA PU = MPU

-- -- instance PU.Unbox e => MutableArray MPU (Int, Int) e where

-- --   thawA (PUArray arr) = MPUArray <$> thawVG arr
-- --   {-# INLINE thawA #-}

-- --   freezeA (MPUArray arr) = PUArray <$> freezeVG arr
-- --   {-# INLINE freezeA #-}

-- --   mshapeA (MPUArray marr) = mdimsVG marr
-- --   {-# INLINE mshapeA #-}

-- --   newA !ix = MPUArray <$> newVG ix
-- --   {-# INLINE newA #-}

-- --   readA (MPUArray arr) = readVG arr
-- --   {-# INLINE readA #-}

-- --   writeA (MPUArray arr) = writeVG arr
-- --   {-# INLINE writeA #-}

-- --   swapA (MPUArray arr) = swapVG arr
-- --   {-# INLINE swapA #-}


instance I.ColorSpace cs e => Eq (PU (Int,Int) (I.Pixel cs e)) where
   --(PUArray arr1) == (PUArray arr2) = arr1 == arr2
  -- foldAll (&&) True $ zipWith (==) arr1 arr2


instance I.ColorSpace cs e => I.Array PU cs e where

  toVectorA (PMArray arr) = VU.generate (length arr) (unsafeLinearIndex arr)
  toVectorA iarr = I.toVectorA $ I.computeA iarr
  {-# INLINE toVectorA #-}

  fromVectorA !sz = PMArray . MArray sz . VU.unsafeIndex
  {-# INLINE fromVectorA #-}


-- instance ColorSpace cs e => MArray PU cs e

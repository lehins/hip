{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Graphics.Image.Interface.Repa.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Repa.Unboxed
  ( RSU(..)
  , RPU(..)
  , Repr(..)
  ) where

import qualified Data.Array.Repa.Eval                    as R (Elt)
import qualified Data.Array.Repa.Repr.Unboxed            as R (U)
import           Data.Typeable                           (Typeable)
import qualified Data.Vector.Generic                     as VG (Vector)
import qualified Data.Vector.Unboxed                     as VU (Unbox, Vector)
import           Graphics.Image.Interface                as I
import           Graphics.Image.Interface.Repa.Generic
import           Graphics.Image.Interface.Vector.Generic (makeVectorWindowedVG,
                                                          makeVectorWindowedVGPar)
import           Graphics.Image.Interface.Vector.Unboxed ()
import           Prelude                                 as P


-- | Repa Array representation, which is computed sequentially.
newtype RSU ix px = RSUArray (RArray R.U px) deriving Typeable

data instance Repr RSU = RSU

type instance Vector RSU = VU.Vector


instance (R.Elt e, VU.Unbox e) => BaseArray RSU (Int, Int) e where

  type SuperClass RSU (Int, Int) e = (VG.Vector VU.Vector e, VU.Unbox e, R.Elt e)

  unsafeIndexA (RSUArray v) = unsafeIndexR v
  {-# INLINE unsafeIndexA #-}

  shapeA (RSUArray v) = dimsR v
  {-# INLINE shapeA #-}

  makeA !sz = RSUArray . makeArrayR sz
  {-# INLINE makeA #-}

instance BaseArray RSU (Int, Int) e => IArray RSU (Int, Int) e where

  makeWindowedA !sh !wIx !wSz f g =
    RSUArray $ fromVectorUnboxedR sh $ makeVectorWindowedVG sh wIx wSz f g
  {-# INLINE makeWindowedA #-}

  scalarA = RSUArray . scalarR
  {-# INLINE scalarA #-}

  mapA f (RSUArray v) = RSUArray $ mapR f v
  {-# INLINE mapA #-}

  imapA f (RSUArray v) = RSUArray $ imapR f v
  {-# INLINE imapA #-}

  zipWithA f (RSUArray v1) (RSUArray v2) = RSUArray $ zipWithR f v1 v2
  {-# INLINE zipWithA #-}

  izipWithA f (RSUArray v1) (RSUArray v2) = RSUArray $ izipWithR f v1 v2
  {-# INLINE izipWithA #-}

  traverseA (RSUArray v) f g = RSUArray $ traverseR v f g
  {-# INLINE traverseA #-}

  traverse2A (RSUArray v1) (RSUArray v2) f g = RSUArray $ traverse2R v1 v2 f g
  {-# INLINE traverse2A #-}

  transposeA (RSUArray v) = RSUArray $ transposeR v
  {-# INLINE transposeA #-}

  backpermuteA !sz f (RSUArray v) = RSUArray $ backpermuteR sz f v
  {-# INLINE backpermuteA #-}

  fromListsA = RSUArray . fromListsR
  {-# NOINLINE fromListsA #-}

  foldA f !px0 (RSUArray v) = foldR Sequential f px0 v
  {-# INLINE foldA #-}

  foldIxA f !px0 (RSUArray v) = foldIxR Sequential f px0 v
  {-# INLINE foldIxA #-}

  multA (RSUArray v1) (RSUArray v2) = RSUArray (multR Sequential v1 v2)
  {-# INLINE multA #-}

  computeA (RSUArray arr) = RSUArray (computeR Sequential arr)
  {-# INLINE computeA #-}



instance ColorSpace cs e => Eq (RSU (Int,Int) (Pixel cs e)) where
  (RSUArray arr1) == (RSUArray arr2) = eqR Parallel arr1 arr2


instance (R.Elt e, ColorSpace cs e) => Array RSU cs e where

  toVectorA (RSUArray v) = toVectorUnboxedR Sequential v
  {-# INLINE toVectorA #-}

  fromVectorA !sz = RSUArray . fromVectorUnboxedR sz
  {-# INLINE fromVectorA #-}





-- | Repa Array representation, which is computed in parallel.
newtype RPU ix px = RPUArray (RArray R.U px) deriving Typeable

data instance Repr RPU = RPU

type instance Vector RPU = VU.Vector


instance (R.Elt e, VU.Unbox e) => BaseArray RPU (Int, Int) e where

  type SuperClass RPU (Int, Int) e = (VG.Vector VU.Vector e, VU.Unbox e, R.Elt e)

  unsafeIndexA (RPUArray v) = unsafeIndexR v
  {-# INLINE unsafeIndexA #-}

  shapeA (RPUArray v) = dimsR v
  {-# INLINE shapeA #-}

  makeA !sz = RPUArray . makeArrayR sz
  {-# INLINE makeA #-}

instance BaseArray RPU (Int, Int) e => IArray RPU (Int, Int) e where

  makeWindowedA !sh !wIx !wSz f g =
    RPUArray $ fromVectorUnboxedR sh $ makeVectorWindowedVGPar sh wIx wSz f g
  {-# INLINE makeWindowedA #-}

  scalarA = RPUArray . scalarR
  {-# INLINE scalarA #-}

  mapA f (RPUArray v) = RPUArray $ mapR f v
  {-# INLINE mapA #-}

  imapA f (RPUArray v) = RPUArray $ imapR f v
  {-# INLINE imapA #-}

  zipWithA f (RPUArray v1) (RPUArray v2) = RPUArray $ zipWithR f v1 v2
  {-# INLINE zipWithA #-}

  izipWithA f (RPUArray v1) (RPUArray v2) = RPUArray $ izipWithR f v1 v2
  {-# INLINE izipWithA #-}

  traverseA (RPUArray v) f g = RPUArray $ traverseR v f g
  {-# INLINE traverseA #-}

  traverse2A (RPUArray v1) (RPUArray v2) f g = RPUArray $ traverse2R v1 v2 f g
  {-# INLINE traverse2A #-}

  transposeA (RPUArray v) = RPUArray $ transposeR v
  {-# INLINE transposeA #-}

  backpermuteA !sz f (RPUArray v) = RPUArray $ backpermuteR sz f v
  {-# INLINE backpermuteA #-}

  fromListsA = RPUArray . fromListsR
  {-# NOINLINE fromListsA #-}

  foldA f !px0 (RPUArray v) = foldR Parallel f px0 v
  {-# INLINE foldA #-}

  foldIxA f !px0 (RPUArray v) = foldIxR Parallel f px0 v
  {-# INLINE foldIxA #-}

  multA (RPUArray v1) (RPUArray v2) = RPUArray (multR Parallel v1 v2)
  {-# INLINE multA #-}

  computeA (RPUArray arr) = RPUArray (computeR Parallel arr)
  {-# INLINE computeA #-}




instance ColorSpace cs e => Eq (RPU (Int,Int) (Pixel cs e)) where
  (RPUArray arr1) == (RPUArray arr2) = eqR Parallel arr1 arr2


instance (R.Elt e, ColorSpace cs e) => Array RPU cs e where

  toVectorA (RPUArray v) = toVectorUnboxedR Parallel v
  {-# INLINE toVectorA #-}

  fromVectorA !sz = RPUArray . fromVectorUnboxedR sz
  {-# INLINE fromVectorA #-}



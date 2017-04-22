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
-- Module      : Graphics.Image.Interface.Repa.Storable
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Repa.Storable
  ( RSS
  , RPS
  , Repr(..)
  ) where

import qualified Data.Array.Repa.Eval                    as R (Elt)
import           Data.Array.Repa.Repr.ForeignPtr         as R (F)
import           Data.Typeable                           (Typeable)
import qualified Data.Vector.Generic                     as VG (Vector)
import qualified Data.Vector.Storable                    as VS (Storable,
                                                                Vector)
import qualified Data.Vector.Unboxed                     as VU (Unbox)
import           Graphics.Image.Interface                as I
import           Graphics.Image.Interface.Repa.Generic
import           Graphics.Image.Interface.Vector.Generic (makeVectorWindowedVG,
                                                          makeVectorWindowedVGPar)
import           Graphics.Image.Interface.Vector.Unboxed ()
import           Prelude                                 as P


-- | Repa Array representation, which is computed sequentially.
newtype RSS ix px = RSSArray (RArray R.F px) deriving Typeable

data instance Repr RSS = RSS


type instance Vector RSS = VS.Vector


instance (R.Elt e, VU.Unbox e, VS.Storable e) => BaseArray RSS (Int, Int) e where

  type SuperClass RSS (Int, Int) e = (VG.Vector VS.Vector e, R.Elt e, VU.Unbox e, VS.Storable e)

  unsafeIndexA (RSSArray v) = unsafeIndexR v
  {-# INLINE unsafeIndexA #-}

  shapeA (RSSArray v) = dimsR v
  {-# INLINE shapeA #-}

  makeA !sz = RSSArray . makeArrayR sz
  {-# INLINE makeA #-}

instance BaseArray RSS (Int, Int) e => IArray RSS (Int, Int) e where

  makeWindowedA !sh !wIx !wSz f g =
    RSSArray $ fromVectorStorableR sh $ makeVectorWindowedVG sh wIx wSz f g
  {-# INLINE makeWindowedA #-}

  scalarA = RSSArray . scalarR
  {-# INLINE scalarA #-}

  mapA f (RSSArray v) = RSSArray $ mapR f v
  {-# INLINE mapA #-}

  imapA f (RSSArray v) = RSSArray $ imapR f v
  {-# INLINE imapA #-}

  zipWithA f (RSSArray v1) (RSSArray v2) = RSSArray $ zipWithR f v1 v2
  {-# INLINE zipWithA #-}

  izipWithA f (RSSArray v1) (RSSArray v2) = RSSArray $ izipWithR f v1 v2
  {-# INLINE izipWithA #-}

  traverseA (RSSArray v) f g = RSSArray $ traverseR v f g
  {-# INLINE traverseA #-}

  traverse2A (RSSArray v1) (RSSArray v2) f g = RSSArray $ traverse2R v1 v2 f g
  {-# INLINE traverse2A #-}

  transposeA (RSSArray v) = RSSArray $ transposeR v
  {-# INLINE transposeA #-}

  backpermuteA !sz f (RSSArray v) = RSSArray $ backpermuteR sz f v
  {-# INLINE backpermuteA #-}

  fromListsA = RSSArray . fromListsR
  {-# NOINLINE fromListsA #-}

  foldA f !px0 (RSSArray v) = foldR Sequential f px0 v
  {-# INLINE foldA #-}

  foldIxA f !px0 (RSSArray v) = foldIxR Sequential f px0 v
  {-# INLINE foldIxA #-}

  multA (RSSArray v1) (RSSArray v2) = RSSArray (multR Sequential v1 v2)
  {-# INLINE multA #-}

  computeA (RSSArray arr) = RSSArray (computeR Sequential arr)
  {-# INLINE computeA #-}



instance (VS.Storable (Pixel cs e), ColorSpace cs e) => Eq (RSS (Int,Int) (Pixel cs e)) where
  (RSSArray arr1) == (RSSArray arr2) = eqR Parallel arr1 arr2


instance (R.Elt e, VU.Unbox (Pixel cs e), VS.Storable (Pixel cs e), ColorSpace cs e) =>
  Array RSS cs e where

  toVectorA (RSSArray v) = toVectorStorableR Sequential v
  {-# INLINE toVectorA #-}

  fromVectorA !sz = RSSArray . fromVectorStorableR sz
  {-# INLINE fromVectorA #-}





-- | Repa Array representation, which is computed in parallel.
newtype RPS ix px = RPSArray (RArray R.F px) deriving Typeable

data instance Repr RPS = RPS

type instance Vector RPS = VS.Vector


instance (R.Elt e, VU.Unbox e, VS.Storable e) => BaseArray RPS (Int, Int) e where

  type SuperClass RPS (Int, Int) e = (VG.Vector VS.Vector e, R.Elt e, VU.Unbox e, VS.Storable e)

  unsafeIndexA (RPSArray v) = unsafeIndexR v
  {-# INLINE unsafeIndexA #-}

  shapeA (RPSArray v) = dimsR v
  {-# INLINE shapeA #-}

  makeA !sz = RPSArray . makeArrayR sz
  {-# INLINE makeA #-}

instance BaseArray RPS (Int, Int) e => IArray RPS (Int, Int) e where

  makeWindowedA !sh !wIx !wSz f g =
    RPSArray $ fromVectorStorableR sh $ makeVectorWindowedVGPar sh wIx wSz f g
  {-# INLINE makeWindowedA #-}

  scalarA = RPSArray . scalarR
  {-# INLINE scalarA #-}

  mapA f (RPSArray v) = RPSArray $ mapR f v
  {-# INLINE mapA #-}

  imapA f (RPSArray v) = RPSArray $ imapR f v
  {-# INLINE imapA #-}

  zipWithA f (RPSArray v1) (RPSArray v2) = RPSArray $ zipWithR f v1 v2
  {-# INLINE zipWithA #-}

  izipWithA f (RPSArray v1) (RPSArray v2) = RPSArray $ izipWithR f v1 v2
  {-# INLINE izipWithA #-}

  traverseA (RPSArray v) f g = RPSArray $ traverseR v f g
  {-# INLINE traverseA #-}

  traverse2A (RPSArray v1) (RPSArray v2) f g = RPSArray $ traverse2R v1 v2 f g
  {-# INLINE traverse2A #-}

  transposeA (RPSArray v) = RPSArray $ transposeR v
  {-# INLINE transposeA #-}

  backpermuteA !sz f (RPSArray v) = RPSArray $ backpermuteR sz f v
  {-# INLINE backpermuteA #-}

  fromListsA = RPSArray . fromListsR
  {-# NOINLINE fromListsA #-}

  foldA f !px0 (RPSArray v) = foldR Parallel f px0 v
  {-# INLINE foldA #-}

  foldIxA f !px0 (RPSArray v) = foldIxR Parallel f px0 v
  {-# INLINE foldIxA #-}

  multA (RPSArray v1) (RPSArray v2) = RPSArray (multR Parallel v1 v2)
  {-# INLINE multA #-}

  computeA (RPSArray arr) = RPSArray (computeR Parallel arr)
  {-# INLINE computeA #-}



instance (VS.Storable (Pixel cs e), ColorSpace cs e) => Eq (RPS (Int,Int) (Pixel cs e)) where
  (RPSArray arr1) == (RPSArray arr2) = eqR Parallel arr1 arr2


instance (R.Elt e, VU.Unbox (Pixel cs e), VS.Storable (Pixel cs e), ColorSpace cs e) =>
  Array RPS cs e where

  toVectorA (RPSArray v) = toVectorStorableR Parallel v
  {-# INLINE toVectorA #-}

  fromVectorA !sz = RPSArray . fromVectorStorableR sz
  {-# INLINE fromVectorA #-}

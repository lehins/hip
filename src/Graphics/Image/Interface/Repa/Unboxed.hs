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
module Graphics.Image.Interface.Repa.Unboxed (
  RSU(..), RPU(..), Image(..)
  ) where

import qualified Data.Array.Repa                         as R
import qualified Data.Array.Repa.Eval                    as R
import           Data.Typeable                           (Typeable)
import           Prelude                                 as P

import           Graphics.Image.Interface                as I
import           Graphics.Image.Interface.Repa.Generic
import qualified Graphics.Image.Interface.Vector.Unboxed as IVU


-- | Repa Array representation backed by Unboxed Vector, which is computed sequentially.
data RSU = RSU deriving Typeable

-- | Repa Array representation backed by Unboxed Vector, which is computed in parallel.
data RPU = RPU deriving Typeable

instance Show RSU where
  show _ = "RepaSequentialUnboxed"

instance Show RPU where
  show _ = "RepaParallelUnboxed"


instance SuperClass RSU cs e => BaseArray RSU cs e where
  type SuperClass RSU cs e =
    (ColorSpace cs e,
     R.Elt e, R.Elt (Pixel cs e))

  newtype Image RSU cs e = SUImage (RImage R.U (Pixel cs e))

  dims (SUImage img) = dimsR img
  {-# INLINE dims #-}


instance (BaseArray RSU cs e) => Array RSU cs e where

  type Manifest RSU = IVU.VU

  type Vector RSU = Vector IVU.VU

  makeImage !sz f = SUImage (makeImageR sz f)
  {-# INLINE makeImage #-}

  makeImageWindowed !sz !wIx !wSz f = SUImage . makeImageWindowedR sz wIx wSz f
  {-# INLINE makeImageWindowed #-}

  scalar = SUImage . scalarR
  {-# INLINE scalar #-}

  index00 (SUImage img) = index00R img
  {-# INLINE index00 #-}

  map f (SUImage img) = SUImage (mapR f img)
  {-# INLINE map #-}

  imap f (SUImage img) = SUImage (imapR f img)
  {-# INLINE imap #-}

  zipWith f (SUImage img1) (SUImage img2) = SUImage (zipWithR f img1 img2)
  {-# INLINE zipWith #-}

  izipWith f (SUImage img1) (SUImage img2) = SUImage (izipWithR f img1 img2)
  {-# INLINE izipWith #-}

  traverse (SUImage img) f g = SUImage (traverseR img f g)
  {-# INLINE traverse #-}

  traverse2 (SUImage img1) (SUImage img2) f g = SUImage (traverse2R img1 img2 f g)
  {-# INLINE traverse2 #-}

  transpose (SUImage img) = SUImage (transposeR img)
  {-# INLINE transpose #-}

  backpermute !sz g (SUImage img) = SUImage (backpermuteR sz g img)
  {-# INLINE backpermute #-}

  fromLists = SUImage . fromListsR
  {-# INLINE fromLists #-}

  fold f !px0 (SUImage img) = foldR Sequential f px0 img
  {-# INLINE fold #-}

  foldIx f !px0 (SUImage img) = foldIxR Sequential f px0 img
  {-# INLINE foldIx #-}

  eq (SUImage img1) (SUImage img2) = eqR Sequential img1 img2
  {-# INLINE eq #-}

  compute (SUImage img) = SUImage (computeR Sequential img)
  {-# INLINE compute #-}

  (|*|) (SUImage img1) (SUImage img2) = SUImage (multR Sequential img1 img2)
  {-# INLINE (|*|) #-}

  toManifest (SUImage img) = fromVector (dimsR img) (toVectorUnboxedR Sequential img)
  {-# INLINE toManifest #-}

  toVector (SUImage img) = toVectorUnboxedR Sequential img
  {-# INLINE toVector #-}

  fromVector !sz = SUImage . fromVectorUnboxedR sz
  {-# INLINE fromVector #-}



instance SuperClass RPU cs e => BaseArray RPU cs e where
  type SuperClass RPU cs e =
    (ColorSpace cs e, R.Elt e, R.Elt (Pixel cs e))

  newtype Image RPU cs e = PUImage (RImage R.U (Pixel cs e))

  dims (PUImage img) = dimsR img
  {-# INLINE dims #-}


instance BaseArray RPU cs e => Array RPU cs e where

  type Manifest RPU = IVU.VU

  type Vector RPU = Vector IVU.VU

  makeImage !sz f = PUImage (makeImageR sz f)
  {-# INLINE makeImage #-}

  makeImageWindowed !sz !wIx !wSz f = PUImage . makeImageWindowedR sz wIx wSz f
  {-# INLINE makeImageWindowed #-}

  scalar = PUImage . scalarR
  {-# INLINE scalar #-}

  index00 (PUImage img) = index00R img
  {-# INLINE index00 #-}

  map f (PUImage img) = PUImage (mapR f img)
  {-# INLINE map #-}

  imap f (PUImage img) = PUImage (imapR f img)
  {-# INLINE imap #-}

  zipWith f (PUImage img1) (PUImage img2) = PUImage (zipWithR f img1 img2)
  {-# INLINE zipWith #-}

  izipWith f (PUImage img1) (PUImage img2) = PUImage (izipWithR f img1 img2)
  {-# INLINE izipWith #-}

  traverse (PUImage img) f g = PUImage (traverseR img f g)
  {-# INLINE traverse #-}

  traverse2 (PUImage img1) (PUImage img2) f g = PUImage (traverse2R img1 img2 f g)
  {-# INLINE traverse2 #-}

  transpose (PUImage img) = PUImage (transposeR img)
  {-# INLINE transpose #-}

  backpermute !sz g (PUImage img) = PUImage (backpermuteR sz g img)
  {-# INLINE backpermute #-}

  fromLists = PUImage . fromListsR
  {-# INLINE fromLists #-}

  fold f !px0 (PUImage img) = foldR Parallel f px0 img
  {-# INLINE fold #-}

  foldIx f !px0 (PUImage img) = foldIxR Parallel f px0 img
  {-# INLINE foldIx #-}

  eq (PUImage img1) (PUImage img2) = eqR Parallel img1 img2
  {-# INLINE eq #-}

  compute (PUImage img) = PUImage (computeR Parallel img)
  {-# INLINE compute #-}

  (|*|) (PUImage img1) (PUImage img2) = PUImage (multR Parallel img1 img2)
  {-# INLINE (|*|) #-}


  toManifest (PUImage img) = fromVector (dimsR img) (toVectorUnboxedR Parallel img)
  {-# INLINE toManifest #-}

  toVector (PUImage img) = toVectorUnboxedR Parallel img
  {-# INLINE toVector #-}

  fromVector !sz = PUImage . fromVectorUnboxedR sz
  {-# INLINE fromVector #-}

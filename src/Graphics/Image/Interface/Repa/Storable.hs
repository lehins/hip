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
module Graphics.Image.Interface.Repa.Storable (
  RSS(..), RPS(..), Image(..)
  ) where

import qualified Data.Array.Repa.Eval                     as R
import qualified Data.Array.Repa.Repr.ForeignPtr          as R
import           Data.Typeable                            (Typeable)
import           Data.Vector.Unboxed                      (Unbox)
import           Foreign.Storable
import           Graphics.Image.Interface                 as I
import           Graphics.Image.Interface.Repa.Generic
import qualified Graphics.Image.Interface.Vector.Storable as IVS
import           Prelude                                  as P




-- | Repa Array representation backed by Storable Vector, which is computed sequentially.
data RSS = RSS deriving Typeable

-- | Repa Array representation backed by Storable Vector, which is computed in parallel.
data RPS = RPS deriving Typeable

instance Show RSS where
  show _ = "RepaSequentialStorable"

instance Show RPS where
  show _ = "RepaParallelStorable"


instance SuperClass RSS cs e => BaseArray RSS cs e where
  type SuperClass RSS cs e =
    (ColorSpace cs e, Unbox (Pixel cs e),
     Storable e, Storable (Pixel cs e),
     R.Elt e, R.Elt (Pixel cs e))

  newtype Image RSS cs e = SSImage (RImage R.F (Pixel cs e))

  dims (SSImage img) = dimsR img
  {-# INLINE dims #-}


instance BaseArray RSS cs e => Array RSS cs e where

  type Manifest RSS = IVS.VS

  type Vector RSS = Vector IVS.VS

  makeImage !sz f = SSImage (makeImageR sz f)
  {-# INLINE makeImage #-}

  makeImageWindowed !sz !wIx !wSz f = SSImage . makeImageWindowedR sz wIx wSz f
  {-# INLINE makeImageWindowed #-}

  scalar = SSImage . scalarR
  {-# INLINE scalar #-}

  index00 (SSImage img) = index00R img
  {-# INLINE index00 #-}

  map f (SSImage img) = SSImage (mapR f img)
  {-# INLINE map #-}

  imap f (SSImage img) = SSImage (imapR f img)
  {-# INLINE imap #-}

  zipWith f (SSImage img1) (SSImage img2) = SSImage (zipWithR f img1 img2)
  {-# INLINE zipWith #-}

  izipWith f (SSImage img1) (SSImage img2) = SSImage (izipWithR f img1 img2)
  {-# INLINE izipWith #-}

  traverse (SSImage img) f g = SSImage (traverseR img f g)
  {-# INLINE traverse #-}

  traverse2 (SSImage img1) (SSImage img2) f g = SSImage (traverse2R img1 img2 f g)
  {-# INLINE traverse2 #-}

  transpose (SSImage img) = SSImage (transposeR img)
  {-# INLINE transpose #-}

  backpermute !sz g (SSImage img) = SSImage (backpermuteR sz g img)
  {-# INLINE backpermute #-}

  fromLists = SSImage . fromListsR
  {-# INLINE fromLists #-}

  fold f !px0 (SSImage img) = foldR Sequential f px0 img
  {-# INLINE fold #-}

  foldIx f !px0 (SSImage img) = foldIxR Sequential f px0 img
  {-# INLINE foldIx #-}

  eq (SSImage img1) (SSImage img2) = eqR Sequential img1 img2
  {-# INLINE eq #-}

  compute (SSImage img) = SSImage (computeR Sequential img)
  {-# INLINE compute #-}

  (|*|) (SSImage img1) (SSImage img2) = SSImage (multR Sequential img1 img2)
  {-# INLINE (|*|) #-}

  toManifest (SSImage img) = fromVector (dimsR img) $ toVectorStorableR Sequential img
  {-# INLINE toManifest #-}

  toVector (SSImage img) = toVectorStorableR Sequential img
  {-# INLINE toVector #-}

  fromVector !sz = SSImage . fromVectorStorableR sz
  {-# INLINE fromVector #-}


instance SuperClass RPS cs e => BaseArray RPS cs e where
  type SuperClass RPS cs e =
    (ColorSpace cs e,
     Storable e, Storable (Pixel cs e), Unbox (Pixel cs e),
     R.Elt e, R.Elt (Pixel cs e))

  newtype Image RPS cs e = PSImage (RImage R.F (Pixel cs e))

  dims (PSImage img) = dimsR img
  {-# INLINE dims #-}


instance BaseArray RPS cs e => Array RPS cs e where

  type Manifest RPS = IVS.VS

  type Vector RPS = Vector IVS.VS

  makeImage !sz f = PSImage (makeImageR sz f)
  {-# INLINE makeImage #-}

  makeImageWindowed !sz !wIx !wSz f = PSImage . makeImageWindowedR sz wIx wSz f
  {-# INLINE makeImageWindowed #-}

  scalar = PSImage . scalarR
  {-# INLINE scalar #-}

  index00 (PSImage img) = index00R img
  {-# INLINE index00 #-}

  map f (PSImage img) = PSImage (mapR f img)
  {-# INLINE map #-}

  imap f (PSImage img) = PSImage (imapR f img)
  {-# INLINE imap #-}

  zipWith f (PSImage img1) (PSImage img2) = PSImage (zipWithR f img1 img2)
  {-# INLINE zipWith #-}

  izipWith f (PSImage img1) (PSImage img2) = PSImage (izipWithR f img1 img2)
  {-# INLINE izipWith #-}

  traverse (PSImage img) f g = PSImage (traverseR img f g)
  {-# INLINE traverse #-}

  traverse2 (PSImage img1) (PSImage img2) f g = PSImage (traverse2R img1 img2 f g)
  {-# INLINE traverse2 #-}

  transpose (PSImage img) = PSImage (transposeR img)
  {-# INLINE transpose #-}

  backpermute !sz g (PSImage img) = PSImage (backpermuteR sz g img)
  {-# INLINE backpermute #-}

  fromLists = PSImage . fromListsR
  {-# INLINE fromLists #-}

  fold f !px0 (PSImage img) = foldR Parallel f px0 img
  {-# INLINE fold #-}

  foldIx f !px0 (PSImage img) = foldIxR Parallel f px0 img
  {-# INLINE foldIx #-}

  eq (PSImage img1) (PSImage img2) = eqR Parallel img1 img2
  {-# INLINE eq #-}

  compute (PSImage img) = PSImage (computeR Parallel img)
  {-# INLINE compute #-}

  (|*|) (PSImage img1) (PSImage img2) = PSImage (multR Parallel img1 img2)
  {-# INLINE (|*|) #-}

  toManifest (PSImage img) = fromVector (dimsR img) $ toVectorStorableR Parallel img
  {-# INLINE toManifest #-}

  toVector (PSImage img) = toVectorStorableR Parallel img
  {-# INLINE toVector #-}

  fromVector !sz = PSImage . fromVectorStorableR sz
  {-# INLINE fromVector #-}

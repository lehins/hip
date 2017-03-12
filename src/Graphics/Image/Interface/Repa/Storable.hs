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

import qualified Data.Array.Repa                          as R
import qualified Data.Array.Repa.Eval                     as R
import           Data.Array.Repa.Index
import qualified Data.Array.Repa.Repr.ForeignPtr          as R
import           Data.Typeable                            (Typeable)
import qualified Data.Vector.Storable                     as VS
import           Foreign.Storable
import           Prelude                                  as P

import           Graphics.Image.Interface                 as I
import           Graphics.Image.Interface.Repa.Generic
import qualified Graphics.Image.Interface.Vector.Storable as IVS




-- | Repa Array representation backed by Storable Vector, which is computed sequentially.
data RSS = RSS deriving Typeable

-- | Repa Array representation backed by Storable Vector, which is computed in parallel.
data RPS = RPS deriving Typeable

instance Show RSS where
  show _ = "RepaSequentialStorable"

instance Show RPS where
  show _ = "RepaParallelStorable"


type instance Repr IVS.VS = R.F


instance SuperClass RSS cs e => BaseArray RSS cs e where
  type SuperClass RSS cs e =
    (ColorSpace cs e,
     Storable e, Storable (Pixel cs e),
     R.Elt e, R.Elt (Pixel cs e))

  newtype Image RSS cs e = SSImage (Image (RS IVS.VS) cs e)

  dims (SSImage img) = dims img
  {-# INLINE dims #-}


instance BaseArray RSS cs e => Array RSS cs e where

  type Manifest RSS = IVS.VS

  type Vector RSS = Vector IVS.VS

  makeImage !sz f = SSImage (makeImage sz f)
  {-# INLINE makeImage #-}

  makeImageWindowed !sz !wIx !wSz f = SSImage . makeImageWindowed sz wIx wSz f
  {-# INLINE makeImageWindowed #-}

  scalar = SSImage . I.scalar
  {-# INLINE scalar #-}

  index00 (SSImage img) = index00 img
  {-# INLINE index00 #-}

  map f (SSImage img) = SSImage (I.map f img)
  {-# INLINE map #-}

  imap f (SSImage img) = SSImage (I.imap f img)
  {-# INLINE imap #-}

  zipWith f (SSImage img1) (SSImage img2) = SSImage (I.zipWith f img1 img2)
  {-# INLINE zipWith #-}

  izipWith f (SSImage img1) (SSImage img2) = SSImage (I.izipWith f img1 img2)
  {-# INLINE izipWith #-}

  traverse (SSImage img) f g = SSImage (I.traverse img f g)
  {-# INLINE traverse #-}

  traverse2 (SSImage img1) (SSImage img2) f g = SSImage (I.traverse2 img1 img2 f g)
  {-# INLINE traverse2 #-}

  transpose (SSImage img) = SSImage (I.transpose img)
  {-# INLINE transpose #-}

  backpermute !sz g (SSImage img) = SSImage (I.backpermute sz g img)
  {-# INLINE backpermute #-}

  fromLists = SSImage . fromLists
  {-# INLINE fromLists #-}

  fold f !px0 (SSImage img) = fold f px0 img
  {-# INLINE fold #-}

  foldIx f !px0 (SSImage img) = foldIx f px0 img
  {-# INLINE foldIx #-}

  eq (SSImage img1) (SSImage img2) = img1 == img2
  {-# INLINE eq #-}

  compute (SSImage img) = SSImage (compute img)
  {-# INLINE compute #-}

  (|*|) (SSImage img1) (SSImage img2) = SSImage (img1 |*| img2)
  {-# INLINE (|*|) #-}

  toManifest (SSImage (SScalar px))  = I.scalar px
  toManifest (SSImage (STImage arr)) = fromRepaArrayStorable arr
  toManifest !img                    = toManifest (compute img)
  {-# INLINE toManifest #-}

  toVector = I.toVector . toManifest
  {-# INLINE toVector #-}

  fromVector !sz = SSImage . STImage . fromVectorStorable sz
  {-# INLINE fromVector #-}


instance SuperClass RPS cs e => BaseArray RPS cs e where
  type SuperClass RPS cs e =
    (ColorSpace cs e,
     Storable e, Storable (Pixel cs e),
     R.Elt e, R.Elt (Pixel cs e))

  newtype Image RPS cs e = PSImage (Image (RP IVS.VS) cs e)

  dims (PSImage img) = dims img
  {-# INLINE dims #-}


instance BaseArray RPS cs e => Array RPS cs e where

  type Manifest RPS = IVS.VS

  type Vector RPS = Vector IVS.VS

  makeImage !sz f = PSImage (makeImage sz f)
  {-# INLINE makeImage #-}

  makeImageWindowed !sz !wIx !wSz f = PSImage . makeImageWindowed sz wIx wSz f
  {-# INLINE makeImageWindowed #-}

  scalar = PSImage . scalar
  {-# INLINE scalar #-}

  index00 (PSImage img) = index00 img
  {-# INLINE index00 #-}

  map f (PSImage img) = PSImage (I.map f img)
  {-# INLINE map #-}

  imap f (PSImage img) = PSImage (I.imap f img)
  {-# INLINE imap #-}

  zipWith f (PSImage img1) (PSImage img2) = PSImage (I.zipWith f img1 img2)
  {-# INLINE zipWith #-}

  izipWith f (PSImage img1) (PSImage img2) = PSImage (I.izipWith f img1 img2)
  {-# INLINE izipWith #-}

  traverse (PSImage img) f g = PSImage (I.traverse img f g)
  {-# INLINE traverse #-}

  traverse2 (PSImage img1) (PSImage img2) f g = PSImage (I.traverse2 img1 img2 f g)
  {-# INLINE traverse2 #-}

  transpose (PSImage img) = PSImage (I.transpose img)
  {-# INLINE transpose #-}

  backpermute !sz g (PSImage img) = PSImage (backpermute sz g img)
  {-# INLINE backpermute #-}

  fromLists = PSImage . fromLists
  {-# INLINE fromLists #-}

  fold f !px0 (PSImage img) = I.fold f px0 img
  {-# INLINE fold #-}

  foldIx f !px0 (PSImage img) = I.foldIx f px0 img
  {-# INLINE foldIx #-}

  eq (PSImage img1) (PSImage img2) = img1 == img2
  {-# INLINE eq #-}

  compute (PSImage img) = PSImage (compute img)
  {-# INLINE compute #-}

  (|*|) (PSImage img1) (PSImage img2) = PSImage (img1 |*| img2)
  {-# INLINE (|*|) #-}

  toManifest (PSImage (PScalar px))  = scalar px
  toManifest (PSImage (PTImage arr)) = fromRepaArrayStorable arr
  toManifest !img                    = toManifest (compute img)
  {-# INLINE toManifest #-}

  toVector = I.toVector . toManifest
  {-# INLINE toVector #-}

  fromVector !sz = PSImage . PTImage . fromVectorStorable sz
  {-# INLINE fromVector #-}


fromRepaArrayStorable
  :: forall cs e.
     Array IVS.VS cs e
  => R.Array R.F DIM2 (Pixel cs e) -> Image IVS.VS cs e
fromRepaArrayStorable !arr =
  fromVector (sh2ix (R.extent arr)) $
  VS.unsafeFromForeignPtr0 (R.toForeignPtr arr) (m * n)
  where
    (Z :. m :. n) = R.extent arr


fromVectorStorable
  :: forall cs e.
     Storable (Pixel cs e)
  => (Int, Int) -> VS.Vector (Pixel cs e) -> R.Array R.F DIM2 (Pixel cs e)
fromVectorStorable !(m, n) !v
  | sz == sz' = R.fromForeignPtr (ix2sh (m, n)) fp
  | otherwise = error $ "fromVectorStorable: (impossible) Vector size mismatch: " ++
                show sz ++ " vs " ++ show sz'
  where
    !(fp, sz) = VS.unsafeToForeignPtr0 v
    !sz' = m * n

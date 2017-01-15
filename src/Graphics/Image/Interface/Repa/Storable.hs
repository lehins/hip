{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
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

import Prelude as P
import Data.Array.Repa.Index
import qualified Data.Array.Repa as R 
import qualified Data.Array.Repa.Eval as R
import qualified Data.Array.Repa.Repr.ForeignPtr as R
import qualified Data.Vector.Storable as VS
import Foreign.Storable

import Graphics.Image.Interface as I
import Graphics.Image.Interface.Repa.Generic
import qualified Graphics.Image.Interface.Vector.Storable as IVS
import qualified Graphics.Image.Interface.Vector.Unboxed as IVU



-- | Repa Array representation backed by Storable Vector, which is computed sequentially. 
data RSS = RSS

-- | Repa Array representation backed by Storable Vector, which is computed in parallel.
data RPS = RPS

instance Show RSS where
  show RSS = "RepaSequentialStorable"

instance Show RPS where
  show RPS = "RepaParallelStorable"
  

type instance Repr (RS IVS.S) = R.F

type instance Repr (RP IVS.S) = R.F


instance SuperClass RSS cs e => BaseArray RSS cs e where
  type SuperClass RSS cs e =
    (ColorSpace cs e, Num (Pixel cs e),
     Storable e, Storable (Pixel cs e),
     R.Elt e, R.Elt (Pixel cs e))
  
  data Image RSS cs e = SSImage !(Image (RS IVS.S) cs e)
                       
  dims (SSImage img) = dims img
  {-# INLINE dims #-}


instance (BaseArray RSS cs e) => Array RSS cs e where

  type Manifest RSS = IVS.VS
  
  makeImage !sz f = SSImage (makeImage sz f)
  {-# INLINE makeImage #-}
 
  makeImageWindowed !sz !w f = SSImage . makeImageWindowed sz w f
  {-# INLINE makeImageWindowed #-}
 
  singleton = SSImage . I.singleton
  {-# INLINE singleton #-}

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

  toManifest (SSImage (SScalar px)) = I.singleton px
  toManifest (SSImage (STImage arr)) = fromRepaArrayStorable arr
  toManifest !img = toManifest (compute img)
  {-# INLINE toManifest #-}



instance SuperClass RPS cs e => BaseArray RPS cs e where
  type SuperClass RPS cs e =
    (ColorSpace cs e, Num (Pixel cs e),
     Storable e, Storable (Pixel cs e),
     IVU.Unbox e, IVU.Unbox (Components cs e),
     R.Elt e, R.Elt (Pixel cs e))
  
  data Image RPS cs e = PSImage !(Image (RP IVS.S) cs e)
                       
  dims (PSImage img) = dims img
  {-# INLINE dims #-}


instance (BaseArray RPS cs e) => Array RPS cs e where

  type Manifest RPS = IVS.VS
  
  makeImage !sz f = PSImage (makeImage sz f)
  {-# INLINE makeImage #-}
 
  makeImageWindowed !sz !w f = PSImage . makeImageWindowed sz w f
  {-# INLINE makeImageWindowed #-}
 
  singleton = PSImage . singleton
  {-# INLINE singleton #-}

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

  toManifest (PSImage (PScalar px)) = singleton px
  toManifest (PSImage (PTImage arr)) = fromRepaArrayStorable arr
  toManifest !img = toManifest (compute img)
  {-# INLINE toManifest #-}


-- | Changes computation strategy. Will casue all fused operations to be computed.
instance Exchangable RPS RSS where
  
  exchange _ (PSImage img) = SSImage (toRS img)
  {-# INLINE exchange #-}


-- | Changes computation strategy. Will casue all fused operations to be computed.
instance Exchangable RSS RPS where
  
  exchange _ (SSImage img) = PSImage (toRP img)
  {-# INLINE exchange #-}


-- | O(1) - Changes to Repa representation.
instance Exchangable IVS.VS RSS where
  exchange _ !img@(dims -> (1, 1)) = singleton (index00 img)
  exchange _ !img =
    SSImage . STImage . toRepaArrayStorable $ img
  {-# INLINE exchange #-}


-- | O(1) - Changes to Repa representation.
instance Exchangable IVS.VS RPS where
  exchange _ !img@(dims -> (1, 1)) = singleton (index00 img)
  exchange _ !img =
    PSImage . PTImage . toRepaArrayStorable $ img
  {-# INLINE exchange #-}


-- | Changes to Vector representation.
instance Exchangable RSS IVS.VS where
  exchange _ = toManifest
  {-# INLINE exchange #-}


-- | Changes to Vector representation.
instance Exchangable RPS IVS.VS where
  exchange _ = toManifest
  {-# INLINE exchange #-}


fromRepaArrayStorable
  :: forall cs e.
     Array IVS.VS cs e
  => R.Array R.F DIM2 (Pixel cs e) -> Image IVS.VS cs e
fromRepaArrayStorable !arr =
  IVS.fromStorableVector (sh2ix (R.extent arr)) $
  VS.unsafeFromForeignPtr0 (R.toForeignPtr arr) sz
  where
    !sz = sizeOf (undefined :: Pixel cs e) * m * n
    (Z :. m :. n) = R.extent arr


toRepaArrayStorable
  :: forall cs e.
     Array IVS.VS cs e
  => Image IVS.VS cs e -> R.Array R.F DIM2 (Pixel cs e)
toRepaArrayStorable !img
  | sz == sz' = R.fromForeignPtr (ix2sh (dims img)) fp
  | otherwise = error $ "toRepaArrayStorable: (impossible) Vector size mismatch: " ++
                show sz ++ " vs " ++ show sz'
  where
    !(fp, sz) = VS.unsafeToForeignPtr0 $ IVS.toStorableVector img
    !sz' = sizeOf (undefined :: Pixel cs e) * m * n
    !(m, n) = dims img

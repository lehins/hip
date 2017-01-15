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

import Prelude as P
import qualified Data.Array.Repa as R 
import qualified Data.Array.Repa.Eval as R

import Graphics.Image.Interface as I
import Graphics.Image.Interface.Repa.Generic
import qualified Graphics.Image.Interface.Vector.Unboxed as IVU


-- | Repa Array representation backed by Unboxed Vector, which is computed sequentially. 
data RSU = RSU

-- | Repa Array representation backed by Unboxed Vector, which is computed in parallel.
data RPU = RPU

instance Show RSU where
  show RSU = "RepaSequentialUnboxed"

instance Show RPU where
  show RPU = "RepaParallelUnboxed"
  

type instance Repr (RS IVU.U) = R.U

type instance Repr (RP IVU.U) = R.U


instance SuperClass RSU cs e => BaseArray RSU cs e where
  type SuperClass RSU cs e =
    (ColorSpace cs e, Num (Pixel cs e),
     IVU.Unbox e, IVU.Unbox (Components cs e),
     R.Elt e, R.Elt (Pixel cs e))
  
  data Image RSU cs e = SUImage !(Image (RS IVU.U) cs e)
                       
  dims (SUImage img) = dims img
  {-# INLINE dims #-}


instance (BaseArray RSU cs e) => Array RSU cs e where

  type Manifest RSU = IVU.VU
  
  makeImage !sz f = SUImage (makeImage sz f)
  {-# INLINE makeImage #-}
 
  makeImageWindowed !sz !w f = SUImage . makeImageWindowed sz w f
  {-# INLINE makeImageWindowed #-}
 
  singleton = SUImage . singleton
  {-# INLINE singleton #-}

  index00 (SUImage img) = index00 img
  {-# INLINE index00 #-}

  map f (SUImage img) = SUImage (I.map f img)
  {-# INLINE map #-}

  imap f (SUImage img) = SUImage (I.imap f img)
  {-# INLINE imap #-}

  zipWith f (SUImage img1) (SUImage img2) = SUImage (I.zipWith f img1 img2)
  {-# INLINE zipWith #-}

  izipWith f (SUImage img1) (SUImage img2) = SUImage (I.izipWith f img1 img2)
  {-# INLINE izipWith #-}

  traverse (SUImage img) f g = SUImage (I.traverse img f g)
  {-# INLINE traverse #-}

  traverse2 (SUImage img1) (SUImage img2) f g = SUImage (I.traverse2 img1 img2 f g)
  {-# INLINE traverse2 #-}

  transpose (SUImage img) = SUImage (I.transpose img)
  {-# INLINE transpose #-}

  backpermute !sz g (SUImage img) = SUImage (backpermute sz g img)
  {-# INLINE backpermute #-}

  fromLists = SUImage . fromLists
  {-# INLINE fromLists #-}

  fold f !px0 (SUImage img) = fold f px0 img
  {-# INLINE fold #-}

  foldIx f !px0 (SUImage img) = foldIx f px0 img
  {-# INLINE foldIx #-}

  eq (SUImage img1) (SUImage img2) = img1 == img2
  {-# INLINE eq #-}

  compute (SUImage img) = SUImage (compute img)
  {-# INLINE compute #-}

  (|*|) (SUImage img1) (SUImage img2) = SUImage (img1 |*| img2)
  {-# INLINE (|*|) #-}

  toManifest (SUImage (SScalar px)) = singleton px
  toManifest (SUImage (STImage arr)) =
    IVU.fromUnboxedVector (sh2ix (R.extent arr)) (R.toUnboxed arr)
  toManifest !img = toManifest (compute img)
  {-# INLINE toManifest #-}



instance SuperClass RPU cs e => BaseArray RPU cs e where
  type SuperClass RPU cs e =
    (ColorSpace cs e, Num (Pixel cs e),
     IVU.Unbox e, IVU.Unbox (Components cs e),
     R.Elt e, R.Elt (Pixel cs e))
  
  data Image RPU cs e = PUImage !(Image (RP IVU.U) cs e)
                       
  dims (PUImage img) = dims img
  {-# INLINE dims #-}


instance (BaseArray RPU cs e) => Array RPU cs e where

  type Manifest RPU = IVU.VU
  
  makeImage !sz f = PUImage (makeImage sz f)
  {-# INLINE makeImage #-}
 
  makeImageWindowed !sz !w f = PUImage . makeImageWindowed sz w f
  {-# INLINE makeImageWindowed #-}
 
  singleton = PUImage . singleton
  {-# INLINE singleton #-}

  index00 (PUImage img) = index00 img
  {-# INLINE index00 #-}

  map f (PUImage img) = PUImage (I.map f img)
  {-# INLINE map #-}

  imap f (PUImage img) = PUImage (I.imap f img)
  {-# INLINE imap #-}

  zipWith f (PUImage img1) (PUImage img2) = PUImage (I.zipWith f img1 img2)
  {-# INLINE zipWith #-}

  izipWith f (PUImage img1) (PUImage img2) = PUImage (I.izipWith f img1 img2)
  {-# INLINE izipWith #-}

  traverse (PUImage img) f g = PUImage (I.traverse img f g)
  {-# INLINE traverse #-}

  traverse2 (PUImage img1) (PUImage img2) f g = PUImage (I.traverse2 img1 img2 f g)
  {-# INLINE traverse2 #-}

  transpose (PUImage img) = PUImage (I.transpose img)
  {-# INLINE transpose #-}

  backpermute !sz g (PUImage img) = PUImage (backpermute sz g img)
  {-# INLINE backpermute #-}

  fromLists = PUImage . fromLists
  {-# INLINE fromLists #-}

  fold f !px0 (PUImage img) = I.fold f px0 img
  {-# INLINE fold #-}

  foldIx f !px0 (PUImage img) = I.foldIx f px0 img
  {-# INLINE foldIx #-}

  eq (PUImage img1) (PUImage img2) = img1 == img2
  {-# INLINE eq #-}

  compute (PUImage img) = PUImage (compute img)
  {-# INLINE compute #-}

  (|*|) (PUImage img1) (PUImage img2) = PUImage (img1 |*| img2)
  {-# INLINE (|*|) #-}

  toManifest (PUImage (PScalar px)) = singleton px
  toManifest (PUImage (PTImage arr)) =
    IVU.fromUnboxedVector (sh2ix (R.extent arr)) (R.toUnboxed arr)
  toManifest !img = toManifest (compute img)
  {-# INLINE toManifest #-}


-- | Changes computation strategy. Will casue all fused operations to be computed.
instance Exchangable RPU RSU where
  
  exchange _ (PUImage img)  = SUImage (toRS img)
  {-# INLINE exchange #-}


-- | Changes computation strategy. Will casue all fused operations to be computed.
instance Exchangable RSU RPU where
  
  exchange _ (SUImage img)  = PUImage (toRP img)
  {-# INLINE exchange #-}


-- | O(1) - Changes to Repa representation.
instance Exchangable IVU.VU RSU where
  exchange _ img@(dims -> (1, 1)) = singleton (index00 img)
  exchange _ img =
    SUImage . STImage . R.fromUnboxed (ix2sh $ dims img) . IVU.toUnboxedVector $ img
  {-# INLINE exchange #-}


-- | O(1) - Changes to Repa representation.
instance Exchangable IVU.VU RPU where
  exchange _ img@(dims -> (1, 1)) = singleton (index00 img)
  exchange _ img =
    PUImage . PTImage . R.fromUnboxed (ix2sh $ dims img) . IVU.toUnboxedVector $ img
  {-# INLINE exchange #-}


-- | Changes to Vector representation.
instance Exchangable RSU IVU.VU where
  exchange _ = toManifest
  {-# INLINE exchange #-}


-- | Changes to Vector representation.
instance Exchangable RPU IVU.VU where
  exchange _ = toManifest
  {-# INLINE exchange #-}

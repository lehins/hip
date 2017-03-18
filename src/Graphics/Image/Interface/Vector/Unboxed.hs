{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : Graphics.Image.Interface.Vector.Unboxed
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Vector.Unboxed (
  VU(..), VU.Unbox, Image(..)
  ) where

import           Control.DeepSeq                          (deepseq)
import           Prelude                                  hiding (map, zipWith)
#if !MIN_VERSION_base(4,8,0)
import           Data.Functor
#endif
import           Data.Typeable                            (Typeable)
import qualified Data.Vector.Unboxed                      as VU
import           Graphics.Image.Interface                 as I
import           Graphics.Image.Interface.Vector.Generic
import           Graphics.Image.Interface.Vector.Unboxing ()



-- | Unboxed 'Vector' representation.
data VU = VU deriving Typeable

instance Show VU where
  show _ = "VectorUnboxed"

instance SuperClass VU cs e => BaseArray VU cs e where
  type SuperClass VU cs e =
    (ColorSpace cs e, VU.Unbox (Components cs e))

  newtype Image VU cs e = VUImage (VGImage VU.Vector (Pixel cs e))

  dims (VUImage img) = dimsVG img
  {-# INLINE dims #-}



instance (MArray VU cs e, BaseArray VU cs e) => Array VU cs e where

  type Manifest VU = VU

  type Vector VU = VU.Vector

  makeImage !sh = VUImage . makeImageVG sh
  {-# INLINE makeImage #-}

  makeImageWindowed !sh !wIx !wSz f g = VUImage $ makeImageWindowedVG sh wIx wSz f g
  {-# INLINE makeImageWindowed #-}

  scalar = VUImage . scalarVG
  {-# INLINE scalar #-}

  index00 (VUImage img) = index00VG img
  {-# INLINE index00 #-}

  map f (VUImage img) = VUImage $ mapVG f img
  {-# INLINE map #-}

  imap f (VUImage img) = VUImage $ imapVG f img
  {-# INLINE imap #-}

  zipWith f (VUImage img1) (VUImage img2) = VUImage $ zipWithVG f img1 img2
  {-# INLINE zipWith #-}

  izipWith f (VUImage img1) (VUImage img2) = VUImage $ izipWithVG f img1 img2
  {-# INLINE izipWith #-}

  traverse (VUImage img) f g = VUImage $ traverseVG img f g
  {-# INLINE traverse #-}

  traverse2 (VUImage img1) (VUImage img2) f g = VUImage $ traverse2VG img1 img2 f g
  {-# INLINE traverse2 #-}

  transpose (VUImage img) = VUImage $ transposeVG img
  {-# INLINE transpose #-}

  backpermute !sz f (VUImage img) = VUImage $ backpermuteVG sz f img
  {-# INLINE backpermute #-}

  fromLists = VUImage . fromListsVG
  {-# NOINLINE fromLists #-}

  fold f !px0 (VUImage img) = foldlVG f px0 img
  {-# INLINE fold #-}

  foldIx f !px0 (VUImage img) = ifoldlVG f px0 img
  {-# INLINE foldIx #-}

  (|*|) (VUImage img1) (VUImage img2) = VUImage (multVG img1 img2)
  {-# INLINE (|*|) #-}

  eq (VUImage img1) (VUImage img2) = img1 == img2
  {-# INLINE eq #-}

  compute (VUImage img) = img `deepseq` VUImage img
  {-# INLINE compute #-}

  toManifest = id
  {-# INLINE toManifest #-}

  toVector (VUImage img) = toVectorVG img
  {-# INLINE toVector #-}

  fromVector !sz = VUImage . fromVectorVG sz
  {-# INLINE fromVector #-}


instance BaseArray VU cs e => MArray VU cs e where

  newtype MImage s VU cs e = MVUImage (MVGImage s VU.Vector (Pixel cs e))

  unsafeIndex (VUImage img) = unsafeIndexVG img
  {-# INLINE unsafeIndex #-}

  deepSeqImage (VUImage img) = deepseq img
  {-# INLINE deepSeqImage #-}

  foldl f !px0 (VUImage img) = foldlVG f px0 img
  {-# INLINE foldl #-}

  foldr f !px0 (VUImage img) = foldrVG f px0 img
  {-# INLINE foldr #-}

  makeImageM !sh f = VUImage <$> makeImageMVG sh f
  {-# INLINE makeImageM #-}

  mapM f (VUImage img) = VUImage <$> mapMVG f img
  {-# INLINE mapM #-}

  mapM_ f (VUImage img) = mapM_VG f img
  {-# INLINE mapM_ #-}

  foldM f !px0 (VUImage img) = foldMVG f px0 img
  {-# INLINE foldM #-}

  foldM_ f !px0 (VUImage img) = foldM_VG f px0 img
  {-# INLINE foldM_ #-}

  mdims (MVUImage mimg) = mdimsVG mimg
  {-# INLINE mdims #-}

  thaw (VUImage img) = MVUImage <$> thawVG img
  {-# INLINE thaw #-}

  freeze (MVUImage img) = VUImage <$> freezeVG img
  {-# INLINE freeze #-}

  new !ix = MVUImage <$> newVG ix
  {-# INLINE new #-}

  read (MVUImage img) = readVG img
  {-# INLINE read #-}

  write (MVUImage img) = writeVG img
  {-# INLINE write #-}

  swap (MVUImage img) = swapVG img
  {-# INLINE swap #-}


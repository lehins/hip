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
-- Module      : Graphics.Image.Interface.Vector.Storable
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Vector.Storable (
  VS(..), Image(..)
  ) where

import           Prelude                                 hiding (map, zipWith)
#if !MIN_VERSION_base(4,8,0)
import           Data.Functor
#endif
import           Control.DeepSeq                         (deepseq)
import           Data.Typeable                           (Typeable)
import qualified Data.Vector.Storable                    as VS
import           Graphics.Image.Interface                as I
import           Graphics.Image.Interface.Vector.Generic



-- | Storable 'Vector' representation.
data VS = VS deriving Typeable

instance Show VS where
  show _ = "VectorStorable"

instance SuperClass VS cs e => BaseArray VS cs e where
  type SuperClass VS cs e =
    (ColorSpace cs e, VS.Storable (Pixel cs e))

  newtype Image VS cs e = VSImage (VGImage VS.Vector (Pixel cs e))

  dims (VSImage img) = dimsVG img
  {-# INLINE dims #-}



instance (MArray VS cs e, BaseArray VS cs e) => Array VS cs e where

  type Manifest VS = VS

  type Vector VS = VS.Vector

  makeImage !sh = VSImage . makeImageVG sh
  {-# INLINE makeImage #-}

  makeImageWindowed !sh !wIx !wSz f g = VSImage $ makeImageWindowedVG sh wIx wSz f g
  {-# INLINE makeImageWindowed #-}

  scalar = VSImage . scalarVG
  {-# INLINE scalar #-}

  index00 (VSImage img) = index00VG img
  {-# INLINE index00 #-}

  map f (VSImage img) = VSImage $ mapVG f img
  {-# INLINE map #-}

  imap f (VSImage img) = VSImage $ imapVG f img
  {-# INLINE imap #-}

  zipWith f (VSImage img1) (VSImage img2) = VSImage $ zipWithVG f img1 img2
  {-# INLINE zipWith #-}

  izipWith f (VSImage img1) (VSImage img2) = VSImage $ izipWithVG f img1 img2
  {-# INLINE izipWith #-}

  traverse (VSImage img) f g = VSImage $ traverseVG img f g
  {-# INLINE traverse #-}

  traverse2 (VSImage img1) (VSImage img2) f g = VSImage $ traverse2VG img1 img2 f g
  {-# INLINE traverse2 #-}

  transpose (VSImage img) = VSImage $ transposeVG img
  {-# INLINE transpose #-}

  backpermute !sz f (VSImage img) = VSImage $ backpermuteVG sz f img
  {-# INLINE backpermute #-}

  fromLists = VSImage . fromListsVG
  {-# INLINE fromLists #-}

  fold f !px0 (VSImage img) = foldlVG f px0 img
  {-# INLINE fold #-}

  foldIx f !px0 (VSImage img) = ifoldlVG f px0 img
  {-# INLINE foldIx #-}

  (|*|) (VSImage img1) (VSImage img2) = VSImage (multVG img1 img2)
  {-# INLINE (|*|) #-}

  eq (VSImage img1) (VSImage img2) = img1 == img2
  {-# INLINE eq #-}

  compute (VSImage img) = img `deepseq` VSImage img
  {-# INLINE compute #-}

  toManifest = id
  {-# INLINE toManifest #-}

  toVector (VSImage img) = toVectorVG img
  {-# INLINE toVector #-}

  fromVector !sz = VSImage . fromVectorVG sz
  {-# INLINE fromVector #-}


instance BaseArray VS cs e => MArray VS cs e where

  newtype MImage s VS cs e = MVSImage (MVGImage s VS.Vector (Pixel cs e))

  unsafeIndex (VSImage img) = unsafeIndexVG img
  {-# INLINE unsafeIndex #-}

  deepSeqImage (VSImage img) = deepseq img
  {-# INLINE deepSeqImage #-}

  foldl f !px0 (VSImage img) = foldlVG f px0 img
  {-# INLINE foldl #-}

  foldr f !px0 (VSImage img) = foldrVG f px0 img
  {-# INLINE foldr #-}

  makeImageM !sh f = VSImage <$> makeImageMVG sh f
  {-# INLINE makeImageM #-}

  mapM f (VSImage img) = VSImage <$> mapMVG f img
  {-# INLINE mapM #-}

  mapM_ f (VSImage img) = mapM_VG f img
  {-# INLINE mapM_ #-}

  foldM f !px0 (VSImage img) = foldMVG f px0 img
  {-# INLINE foldM #-}

  foldM_ f !px0 (VSImage img) = foldM_VG f px0 img
  {-# INLINE foldM_ #-}

  mdims (MVSImage mimg) = mdimsVG mimg
  {-# INLINE mdims #-}

  thaw (VSImage img) = MVSImage <$> thawVG img
  {-# INLINE thaw #-}

  freeze (MVSImage img) = VSImage <$> freezeVG img
  {-# INLINE freeze #-}

  new !ix = MVSImage <$> newVG ix
  {-# INLINE new #-}

  read (MVSImage img) = readVG img
  {-# INLINE read #-}

  write (MVSImage img) = writeVG img
  {-# INLINE write #-}

  swap (MVSImage img) = swapVG img
  {-# INLINE swap #-}

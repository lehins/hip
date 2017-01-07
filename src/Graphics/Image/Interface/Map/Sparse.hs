{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Graphics.Image.Interface.Map.Sparse
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Map.Sparse (
  MS(..), IMS(..), Image(..), 
  ) where

import Prelude as P

import Control.DeepSeq (NFData, deepseq)
import Control.Monad (guard)
import Data.Typeable (Typeable)
import Data.Map.Strict as M
import Data.IntMap.Strict as IM
import Data.Vector.Unboxed (Unbox)

-- import qualified Data.Vector.Unboxed.Mutable as MV
import Graphics.Image.Interface
import Graphics.Image.Interface.Vector.Unboxed
import Graphics.Image.Interface.Vector.Unboxing()


-- | Sparse unboxed matrix representation.
data MS = MS


instance Show MS where
  show _ = "MapSparse"


instance Elt MS cs e => BaseArray MS cs e where
  type Elt MS cs e = (ColorSpace cs, Num e, Unbox e, Typeable e, 
                      Unbox (PixelElt cs e), NFData e, NFData (Pixel cs e),
                      Unbox (Pixel cs e), Eq (Pixel cs e))

  data Image MS cs e = MScalar (Pixel cs e)
                     | MSImage !Int !Int !(Map (Int, Int) (Pixel cs e))

  dims (MScalar _)     = (1, 1)
  dims (MSImage m n _) = (m, n)
  {-# INLINE dims #-}


instance BaseArray MS cs e => Array MS cs e where

  type Manifest MS = VU

  makeImage !(m, n) f =
    MSImage m n $ M.fromDistinctAscList $ do
      i <- [0..m-1]
      j <- [0..n-1]
      let !px = f (i, j)
      guard (px /= 0)
      return ((i,j), px)
  {-# INLINE makeImage #-}

  singleton = MScalar
  {-# INLINE singleton #-}

  index00 (MScalar px) = px
  index00 (MSImage _ _ im) = M.findWithDefault 0 (0, 0) im
  {-# INLINE index00 #-}

  map f (MScalar px)     = MScalar (f px)
  map f (MSImage m n im) = MSImage m n (M.union (M.mapMaybe f' im) im0s) where
    f' !px' = let !px = f px' in
      if px == 0
      then Nothing
      else Just px
    {-# INLINE f' #-}
    im0s = let !px = f 0 in
      if px == 0
      then M.empty
      else M.fromDistinctAscList
           [((i,j), px) | i <- [0..m-1], j <- [0..n-1], M.notMember (i, j) im]
  {-# INLINE map #-}

  
  fromLists !ls = if all (== n) (P.map length ls)
                  then MSImage m n $ M.filter (/=0) $ M.fromDistinctAscList $
                       zip [(i, j) | i <- [0..m-1], j <- [0..n-1]] $ concat ls
                  else error "MS.fromLists: Inner lists are of different lengths."
    where
      !(m, n) = checkDims "MS.fromLists" (length ls, length $ head ls)
  {-# INLINE fromLists #-}

  foldIx f !px0 (MScalar px) = f px0 (0,0) px
  foldIx f !px0 (MSImage _ _ im) = M.foldlWithKey' f px0 im
  {-# INLINE foldIx #-}


  compute (MSImage m n im) = m `seq` n `seq` im `deepseq` (MSImage m n im)
  compute (MScalar px)    = px `seq` MScalar px
  {-# INLINE compute #-}



-- | Sparse IntMap representation.
data IMS = IMS


instance Show IMS where
  show _ = "IntMapSparse"


instance Elt IMS cs e => BaseArray IMS cs e where
  type Elt IMS cs e = (ColorSpace cs, Num e, Unbox e, Typeable e, 
                      Unbox (PixelElt cs e), NFData e, NFData (Pixel cs e),
                      Unbox (Pixel cs e), Eq (Pixel cs e))

  data Image IMS cs e = IMScalar (Pixel cs e)
                     | IMSImage !Int !Int !(IntMap (Pixel cs e))

  dims (IMScalar _)     = (1, 1)
  dims (IMSImage m n _) = (m, n)
  {-# INLINE dims #-}


instance BaseArray IMS cs e => Array IMS cs e where

  type Manifest IMS = VU

  makeImage !(m, n) f =
    IMSImage m n $ IM.fromDistinctAscList $ do
      i <- [0..m-1]
      j <- [0..n-1]
      let !px = f (i, j)
      guard (px /= 0)
      return (fromIx n (i,j), px)
  {-# INLINE makeImage #-}

  singleton = IMScalar
  {-# INLINE singleton #-}

  index00 (IMScalar px) = px
  index00 (IMSImage _ _ im) = IM.findWithDefault 0 0 im
  {-# INLINE index00 #-}

  map f (IMScalar px)     = IMScalar (f px)
  map f (IMSImage m n im) = IMSImage m n (IM.union (IM.mapMaybe f' im) im0s) where
    f' !px' = let !px = f px' in
      if px == 0
      then Nothing
      else Just px
    {-# INLINE f' #-}
    im0s = let !px = f 0 in
      if px == 0
      then IM.empty
      else IM.fromDistinctAscList [(k, px) | k <- [0..(m*n-1)], IM.notMember k im]
  {-# INLINE map #-}

  
  fromLists !ls = if all (== n) (P.map length ls)
                  then IMSImage m n $ IM.filter (/=0) $
                       IM.fromDistinctAscList $ zip [0..] $ concat ls
                  else error "IMS.fromLists: Inner lists are of different lengths."
    where
      !(m, n) = checkDims "IMS.fromLists" (length ls, length $ head ls)
  {-# INLINE fromLists #-}

  foldIx f !px0 (IMScalar px) = f px0 (0,0) px
  foldIx f !px0 (IMSImage _ n im) = IM.foldlWithKey' g px0 im where
    g !acc !k !px = f acc (toIx n k) px
  {-# INLINE foldIx #-}


  compute (IMSImage m n im) = m `seq` n `seq` im `deepseq` (IMSImage m n im)
  compute (IMScalar px)     = px `seq` IMScalar px
  {-# INLINE compute #-}


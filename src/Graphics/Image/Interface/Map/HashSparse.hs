{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Graphics.Image.Interface.Map.HashSparse
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Map.HashSparse (
  HMS(..), Image(..), 
  ) where

import Prelude as P

import Control.DeepSeq (NFData, deepseq)
import Control.Monad (guard)
import Data.Typeable (Typeable)
import Data.HashMap.Strict as M
import Data.Vector.Unboxed (Unbox)

-- import qualified Data.Vector.Unboxed.Mutable as MV
import Graphics.Image.Interface
import Graphics.Image.Interface.Vector.Unboxed
import Graphics.Image.Interface.Vector.Unboxing()


-- | Sparse unboxed matrix representation.
data HMS = HMS


instance Show HMS where
  show _ = "MapSparse"


instance Elt HMS cs e => BaseArray HMS cs e where
  type Elt HMS cs e = (ColorSpace cs, Num e, Unbox e, Typeable e, 
                      Unbox (PixelElt cs e), NFData e, NFData (Pixel cs e),
                      Unbox (Pixel cs e), Eq (Pixel cs e))

  data Image HMS cs e = HMScalar (Pixel cs e)
                     | HMSImage !Int !Int !(HashMap (Int, Int) (Pixel cs e))

  dims (HMScalar _)     = (1, 1)
  dims (HMSImage m n _) = (m, n)
  {-# INLINE dims #-}


instance BaseArray HMS cs e => Array HMS cs e where

  type Manifest HMS = VU

  makeImage !(m, n) f =
    HMSImage m n $ M.fromList $ do
      i <- [0..m-1]
      j <- [0..n-1]
      let !px = f (i, j)
      guard (px /= 0)
      return ((i,j), px)
  {-# INLINE makeImage #-}

  singleton = HMScalar
  {-# INLINE singleton #-}

  index00 (HMScalar px) = px
  index00 (HMSImage _ _ im) = M.lookupDefault 0 (0, 0) im
  {-# INLINE index00 #-}

  map f (HMScalar px)     = HMScalar (f px)
  map f (HMSImage m n im) = HMSImage m n (M.union (M.mapMaybe f' im) im0s) where
    f' !px' = let !px = f px' in
      if px == 0
      then Nothing
      else Just px
    {-# INLINE f' #-}
    im0s = let !px = f 0 in
      if px == 0
      then M.empty
      else M.fromList
           [((i,j), px) | i <- [0..m-1], j <- [0..n-1], not $ M.member (i, j) im]
  {-# INLINE map #-}

  
  fromLists !ls = if all (== n) (P.map length ls)
                  then HMSImage m n $ M.filter (/=0) $ M.fromList $
                       zip [(i, j) | i <- [0..m-1], j <- [0..n-1]] $ concat ls
                  else error "HMS.fromLists: Inner lists are of different lengths."
    where
      !(m, n) = checkDims "HMS.fromLists" (length ls, length $ head ls)
  {-# INLINE fromLists #-}

  foldIx f !px0 (HMScalar px) = f px0 (0,0) px
  foldIx f !px0 (HMSImage _ _ im) = M.foldlWithKey' f px0 im
  {-# INLINE foldIx #-}


  compute (HMSImage m n im) = m `seq` n `seq` im `deepseq` (HMSImage m n im)
  compute (HMScalar px)    = px `seq` HMScalar px
  {-# INLINE compute #-}

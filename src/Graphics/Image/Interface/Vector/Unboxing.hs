{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Graphics.Image.Interface.Vector.Unboxing
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Interface.Vector.Unboxing where

import Data.Word
import Graphics.Image.Interface
import Graphics.Image.ColorSpace.Binary (Bit(..))
import qualified Data.Vector.Generic            as V
import qualified Data.Vector.Generic.Mutable    as M
import qualified Data.Vector.Unboxed            as U
import Control.Monad

-- | Unboxing of a `Bit`.
instance U.Unbox Bit

newtype instance U.MVector s Bit = MV_Bit (U.MVector s Word8)

instance M.MVector U.MVector Bit where
  basicLength (MV_Bit mvec) = M.basicLength mvec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (MV_Bit mvec) = MV_Bit (M.basicUnsafeSlice idx len mvec)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Bit mvec) (MV_Bit mvec') = M.basicOverlaps mvec mvec'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew len = MV_Bit `liftM` M.basicUnsafeNew len
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len (Bit w) = MV_Bit `liftM` M.basicUnsafeReplicate len w
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Bit mvec) idx = Bit `liftM` M.basicUnsafeRead mvec idx
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Bit mvec) idx (Bit w) = M.basicUnsafeWrite mvec idx w
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Bit mvec) = M.basicClear mvec
  {-# INLINE basicClear #-}
  basicSet (MV_Bit mvec) (Bit w) =  M.basicSet mvec w
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Bit mvec) (MV_Bit mvec') = M.basicUnsafeCopy mvec mvec'
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Bit mvec) (MV_Bit mvec') = M.basicUnsafeMove mvec mvec'
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Bit mvec) len = MV_Bit `liftM` M.basicUnsafeGrow mvec len
  {-# INLINE basicUnsafeGrow #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Bit mvec) = M.basicInitialize mvec
  {-# INLINE basicInitialize #-}
#endif


newtype instance U.Vector Bit = V_Bit (U.Vector Word8)

instance V.Vector U.Vector Bit where
  basicUnsafeFreeze (MV_Bit mvec) = V_Bit `liftM` V.basicUnsafeFreeze mvec
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Bit vec) = MV_Bit `liftM` V.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Bit vec) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_Bit vec) = V_Bit (V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Bit vec) idx = Bit `liftM` V.basicUnsafeIndexM vec idx
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Bit mvec) (V_Bit vec) = V.basicUnsafeCopy mvec vec
  {-# INLINE basicUnsafeCopy #-}
  elemseq (V_Bit vec) (Bit w) = V.elemseq vec w
  {-# INLINE elemseq #-}



-- | Unboxing of a `Pixel`.
instance (ColorSpace cs e, U.Unbox (Components cs e)) => U.Unbox (Pixel cs e)

newtype instance U.MVector s (Pixel cs e) = MV_Pixel (U.MVector s (Components cs e))

instance (ColorSpace cs e, U.Unbox (Components cs e)) => M.MVector U.MVector (Pixel cs e) where
  basicLength (MV_Pixel mvec) = M.basicLength mvec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (MV_Pixel mvec) = MV_Pixel (M.basicUnsafeSlice idx len mvec)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Pixel mvec) (MV_Pixel mvec') = M.basicOverlaps mvec mvec'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew len = MV_Pixel `liftM` M.basicUnsafeNew len
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate len val = MV_Pixel `liftM` M.basicUnsafeReplicate len (toComponents val)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Pixel mvec) idx = fromComponents `liftM` M.basicUnsafeRead mvec idx
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Pixel mvec) idx val = M.basicUnsafeWrite mvec idx (toComponents val)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Pixel mvec) = M.basicClear mvec
  {-# INLINE basicClear #-}
  basicSet (MV_Pixel mvec) val = M.basicSet mvec (toComponents val)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Pixel mvec) (MV_Pixel mvec') = M.basicUnsafeCopy mvec mvec'
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Pixel mvec) (MV_Pixel mvec') = M.basicUnsafeMove mvec mvec'
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Pixel mvec) len = MV_Pixel `liftM` M.basicUnsafeGrow mvec len
  {-# INLINE basicUnsafeGrow #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Pixel mvec) = M.basicInitialize mvec
  {-# INLINE basicInitialize #-}
#endif


newtype instance U.Vector (Pixel cs e) = V_Pixel (U.Vector (Components cs e))

instance (ColorSpace cs e, U.Unbox (Components cs e)) => V.Vector U.Vector (Pixel cs e) where
  basicUnsafeFreeze (MV_Pixel mvec) = V_Pixel `liftM` V.basicUnsafeFreeze mvec
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Pixel vec) = MV_Pixel `liftM` V.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Pixel vec) = V.basicLength vec
  {-# INLINE basicLength #-}
  basicUnsafeSlice idx len (V_Pixel vec) = V_Pixel (V.basicUnsafeSlice idx len vec)
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Pixel vec) idx = fromComponents `liftM` V.basicUnsafeIndexM vec idx
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Pixel mvec) (V_Pixel vec) = V.basicUnsafeCopy mvec vec
  {-# INLINE basicUnsafeCopy #-}
  elemseq (V_Pixel vec) val = V.elemseq vec (toComponents val)
  {-# INLINE elemseq #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
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

instance U.Unbox Bit

newtype instance U.MVector s Bit = MV_Bit (U.MVector s Word8)

instance M.MVector U.MVector Bit where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Bit mvec) = M.basicLength mvec
  basicUnsafeSlice idx len (MV_Bit mvec) = MV_Bit (M.basicUnsafeSlice idx len mvec)
  basicOverlaps (MV_Bit mvec) (MV_Bit mvec') = M.basicOverlaps mvec mvec'
  basicUnsafeNew len = MV_Bit `liftM` M.basicUnsafeNew len
  basicUnsafeReplicate len (Bit w) = MV_Bit `liftM` M.basicUnsafeReplicate len w
  basicUnsafeRead (MV_Bit mvec) idx = Bit `liftM` M.basicUnsafeRead mvec idx
  basicUnsafeWrite (MV_Bit mvec) idx (Bit w) = M.basicUnsafeWrite mvec idx w
  basicClear (MV_Bit mvec) = M.basicClear mvec
  basicSet (MV_Bit mvec) (Bit w) =  M.basicSet mvec w
  basicUnsafeCopy (MV_Bit mvec) (MV_Bit mvec') = M.basicUnsafeCopy mvec mvec'
  basicUnsafeMove (MV_Bit mvec) (MV_Bit mvec') = M.basicUnsafeMove mvec mvec'
  basicUnsafeGrow (MV_Bit mvec) len = MV_Bit `liftM` M.basicUnsafeGrow mvec len

    
newtype instance U.Vector Bit = V_Bit (U.Vector Word8)

instance V.Vector U.Vector Bit where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Bit mvec) = V_Bit `liftM` V.basicUnsafeFreeze mvec
  basicUnsafeThaw (V_Bit vec) = MV_Bit `liftM` V.basicUnsafeThaw vec
  basicLength (V_Bit vec) = V.basicLength vec
  basicUnsafeSlice idx len (V_Bit vec) = V_Bit (V.basicUnsafeSlice idx len vec)
  basicUnsafeIndexM (V_Bit vec) idx = Bit `liftM` V.basicUnsafeIndexM vec idx
  basicUnsafeCopy (MV_Bit mvec) (V_Bit vec) = V.basicUnsafeCopy mvec vec
  elemseq (V_Bit vec) (Bit w) = V.elemseq vec w




instance (ColorSpace cs, U.Unbox (PixelElt cs e)) => U.Unbox (Pixel cs e)

newtype instance U.MVector s (Pixel cs e) = MV_Pixel (U.MVector s (PixelElt cs e))
  
instance (ColorSpace cs_aOSR, U.Unbox (PixelElt cs_aOSR e))
         => M.MVector U.MVector (Pixel cs_aOSR e) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Pixel mvec) = M.basicLength mvec
  basicUnsafeSlice idx len (MV_Pixel mvec) = MV_Pixel (M.basicUnsafeSlice idx len mvec)
  basicOverlaps (MV_Pixel mvec) (MV_Pixel mvec') = M.basicOverlaps mvec mvec'
  basicUnsafeNew len = MV_Pixel `liftM` M.basicUnsafeNew len
  basicUnsafeReplicate len val = MV_Pixel `liftM` M.basicUnsafeReplicate len (toElt val)
  basicUnsafeRead (MV_Pixel mvec) idx = fromElt `liftM` M.basicUnsafeRead mvec idx
  basicUnsafeWrite (MV_Pixel mvec) idx val = M.basicUnsafeWrite mvec idx (toElt val)
  basicClear (MV_Pixel mvec) = M.basicClear mvec
  basicSet (MV_Pixel mvec) val = M.basicSet mvec (toElt val)
  basicUnsafeCopy (MV_Pixel mvec) (MV_Pixel mvec') = M.basicUnsafeCopy mvec mvec'
  basicUnsafeMove (MV_Pixel mvec) (MV_Pixel mvec') = M.basicUnsafeMove mvec mvec'
  basicUnsafeGrow (MV_Pixel mvec) len = MV_Pixel `liftM` M.basicUnsafeGrow mvec len

    
newtype instance U.Vector (Pixel cs_aOSR e) = V_Pixel (U.Vector (PixelElt cs_aOSR e))
  
instance (ColorSpace cs, U.Unbox (PixelElt cs e)) => V.Vector U.Vector (Pixel cs e) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Pixel mvec) = V_Pixel `liftM` V.basicUnsafeFreeze mvec
  basicUnsafeThaw (V_Pixel vec) = MV_Pixel `liftM` V.basicUnsafeThaw vec
  basicLength (V_Pixel vec) = V.basicLength vec
  basicUnsafeSlice idx len (V_Pixel vec) = V_Pixel (V.basicUnsafeSlice idx len vec)
  basicUnsafeIndexM (V_Pixel vec) idx = fromElt `liftM` V.basicUnsafeIndexM vec idx
  basicUnsafeCopy (MV_Pixel mvec) (V_Pixel vec) = V.basicUnsafeCopy mvec vec
  elemseq (V_Pixel vec) val = V.elemseq vec (toElt val)

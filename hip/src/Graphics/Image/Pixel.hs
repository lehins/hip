{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell,
             TypeFamilies, UndecidableInstances #-}
-- |
-- Module      : Graphics.Image.Pixel
-- Copyright   : (c) Alexey Kuleshevich 2015
-- License     : MIT
--
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
-- This module contains all available pixel types at the moment.
module Graphics.Image.Pixel (
  -- * Pixel 
  Pixel, P.Channel, pixelGrid,
  -- ** Grayscale
  module HIP.Pixel.Gray, toGray,
  -- ** Color
  -- *** RGB
  module HIP.Pixel.RGB, toRGB,
  -- *** HSI
  module HIP.Pixel.HSI, toHSI,
  -- ** Binary
  module HIP.Binary.Pixel, toBinary,
  -- ** Alpha
  module HIP.Pixel.Alpha, AlphaPixel,
  -- ** Complex
  module HIP.Complex.Pixel,
  ComplexPixel
  ) where

import Data.Int
import Data.Word
import Data.Vector.Unboxed (Unbox)
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
import HIP.Pixel (toGray, toRGB, toHSI, toBinary, pixelGrid, Channel)
import HIP.Pixel.Alpha hiding (AlphaPixel)
import HIP.Pixel.Gray
import HIP.Pixel.RGB
import HIP.Pixel.HSI
import HIP.Binary.Pixel
import HIP.Complex.Pixel hiding (ComplexPixel)
import qualified HIP.Pixel as P (Pixel(..), ComplexPixel, AlphaPixel)

--import qualified Data.Vector.Generic            as V
--import qualified Data.Vector.Generic.Mutable    as M
--import qualified Data.Vector.Unboxed            as U
--import Control.Monad


-- | Unboxed Vector can only work with 'P.Pixel's that implement 'Unbox'
class (Unbox (Channel px), Unbox px, P.Pixel px) => Pixel px where


-- | Unboxed Vector can only work with 'P.Pixel's that implement 'Unbox'. Also
-- Pixel that are instances of this class can be used with 'Alpha' pixel.
class (Unbox px, Unbox (Channel px), P.AlphaPixel px, Pixel px
      ) => AlphaPixel px where

  
-- | Unboxed Vector can only work with 'P.Pixel's that implement 'Unbox'. Also
-- Pixel that are instances of this class can be used with 'Complex' pixel.
class (Unbox px, Unbox (Channel px), P.ComplexPixel px, Pixel px
      ) => ComplexPixel px where
  

-- | Unboxed Pixel  
instance Pixel Binary where

-- | Unboxed Pixel  
instance Pixel Gray where

-- | Unboxed Pixel  
instance Pixel RGB where

-- | Unboxed Pixel  
instance Pixel HSI where
 
-- | Unboxed Pixel  
instance (Unbox (Channel px), AlphaPixel px, Pixel px) => Pixel (Alpha px) where


-- | Unboxed 'AlphaPixel'
instance AlphaPixel Gray where

-- | Unboxed Alpha Pixel  
instance AlphaPixel RGB where

-- | Unboxed Alpha Pixel  
instance AlphaPixel HSI where
  
  
-- | Unboxed Complex Pixel  
instance ComplexPixel px => Pixel (Complex px) where
  
-- | Unboxed Complex Pixel  
instance ComplexPixel Float where

-- | Unboxed Complex Pixel  
instance ComplexPixel Double where
  
-- | Unboxed Complex Pixel  
instance ComplexPixel Gray where

-- | Unboxed Complex Pixel  
instance ComplexPixel RGB where

-- | Unboxed Complex Pixel  
instance ComplexPixel HSI where

-- | Unboxed Complex Pixel  
instance (Pixel (Alpha px), AlphaPixel px, ComplexPixel px) => ComplexPixel (Alpha px) where
  

-- All base types:

instance Pixel Float where

instance Pixel Double where

instance Pixel Int where

instance Pixel Int8 where

instance Pixel Int16 where

instance Pixel Int32 where

instance Pixel Int64 where

instance Pixel Word where

instance Pixel Word8 where

instance Pixel Word16 where

instance Pixel Word32 where

instance Pixel Word64 where

instance AlphaPixel Float where

instance AlphaPixel Double where

instance AlphaPixel Int where

instance AlphaPixel Int8 where

instance AlphaPixel Int16 where

instance AlphaPixel Int32 where

instance AlphaPixel Int64 where

instance AlphaPixel Word where

instance AlphaPixel Word8 where

instance AlphaPixel Word16 where

instance AlphaPixel Word32 where

instance AlphaPixel Word64 where


derivingUnbox "GrayPixel"
    [t| Gray -> Double |]
    [| \(Gray y) -> y  |]
    [| Gray            |]

derivingUnbox "BinaryPixel"
    [t| Binary -> Bool |]
    [| isOn            |]
    [| Binary          |]


derivingUnbox "RGBPixel"
    [t| RGB -> (Double, Double, Double) |]
    [| \(RGB r g b) -> (r, g, b)        |]
    [| \(r, g, b) -> RGB r g b          |]


derivingUnbox "HSIPixel"
    [t| HSI -> (Double, Double, Double) |]
    [| \(HSI h s i) -> (h, s, i)        |]
    [| \(h, s, i) -> HSI h s i          |]


derivingUnbox "ComplexPixel"
    [t| ComplexPixel px => Complex px -> (px, px) |]
    [| \(px1 :+: px2) -> (px1, px2)               |]
    [| uncurry (:+:)                              |]


derivingUnbox "AlphaPixel"
    [t| (AlphaPixel px, Pixel px) => Alpha px -> (Channel px, px) |]
    [| \(Alpha a px) -> (a, px)                                   |]
    [| uncurry Alpha                                              |]


{-  
instance Unbox Gray

newtype instance U.MVector s Gray = MV_Gray (U.MVector s Double)

instance M.MVector U.MVector Gray where
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
  basicLength (MV_Gray v) = M.basicLength v
  basicUnsafeSlice i n (MV_Gray v) = MV_Gray (M.basicUnsafeSlice i n v)
  basicOverlaps (MV_Gray v1) (MV_Gray v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = (MV_Gray `liftM` M.basicUnsafeNew n)
  basicUnsafeReplicate n (Gray y) = MV_Gray `liftM` M.basicUnsafeReplicate n y
  basicUnsafeRead (MV_Gray v) i = Gray `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Gray v) i (Gray y) = M.basicUnsafeWrite v i y
  basicClear (MV_Gray v) = M.basicClear v
  basicSet (MV_Gray v) (Gray y) = M.basicSet v y
  basicUnsafeCopy (MV_Gray v1) (MV_Gray v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Gray v1) (MV_Gray v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Gray v) n = MV_Gray `liftM` M.basicUnsafeGrow v n
          
newtype instance U.Vector Gray = V_Gray (U.Vector Double)

instance V.Vector U.Vector Gray where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Gray v) = V_Gray `liftM` V.basicUnsafeFreeze v
  basicUnsafeThaw (V_Gray v) = MV_Gray `liftM` V.basicUnsafeThaw v
  basicLength (V_Gray v) = V.basicLength v
  basicUnsafeSlice i n (V_Gray v) = V_Gray (V.basicUnsafeSlice i n v)
  basicUnsafeIndexM (V_Gray v) i = Gray  `liftM` V.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Gray mv) (V_Gray v) = V.basicUnsafeCopy mv v
  elemseq (V_Gray v) (Gray y) x = V.elemseq v y x
-}

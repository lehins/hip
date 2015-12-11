{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Graphics.Image.Repa.Pixel (
  Pixel,
  -- * Binary
  Binary, on, off, isOn, isOff,
  -- * Grayscale and Colors
  Gray(..), RGB(..), HSI(..),
  -- * Complex
  Complex(..), ComplexChannel,
  -- * Alpha
  Alpha(..), AlphaChannel, fmapAlpha, combineAlpha
  ) where

import Data.Array.Repa.Eval (Elt(..))
import HIP.Pixel hiding (Pixel, ComplexChannel, AlphaChannel)
import qualified Graphics.Image.Pixel as U (Pixel, ComplexChannel, AlphaChannel)


-- | Repa can only work with 'I.Pixel's that implement 'Elt' and 'Unbox'
class (Elt px, U.Pixel px) => Pixel px where


-- | Repa can only work with 'I.Pixel's that implement 'Elt' and 'Unbox'
class (Elt px, Elt (Channel px), U.ComplexChannel px) =>
      ComplexChannel px where

-- | Repa can only work with 'I.Pixel's that implement 'Elt' and 'Unbox'
class (Elt px, Elt (Channel px), U.AlphaChannel px) =>
      AlphaChannel px where
        

-- | Repa Pixel  
instance Pixel Binary where

-- | Repa Pixel  
instance Pixel Gray where

-- | Repa Pixel  
instance Pixel RGB where

-- | Repa Pixel  
instance Pixel HSI where
 
-- | Repa Pixel  
instance ComplexChannel px => Pixel (Complex px) where

-- | Repa Pixel  
instance AlphaChannel px => Pixel (Alpha px) where
  

-- | Repa Pixel  
instance ComplexChannel Gray where

-- | Repa Pixel  
instance ComplexChannel RGB where

-- | Repa Pixel  
instance ComplexChannel HSI where

  
-- | Repa Pixel  
instance (Pixel (Alpha px), ComplexChannel px, AlphaChannel px) =>
         ComplexChannel (Alpha px) where
  

-- | Repa Pixel  
instance AlphaChannel Gray where

-- | Repa Pixel  
instance AlphaChannel RGB where

-- | Repa Pixel  
instance AlphaChannel HSI where


  
instance Elt Binary where
  touch (Binary (Bin y)) = touch y
  {-# INLINE touch #-}
  
  zero = 0
  {-# INLINE zero #-}

  one = 1
  {-# INLINE one #-}


instance Elt Gray where
  touch (Gray y) = touch y
  {-# INLINE touch #-}
  
  zero = 0
  {-# INLINE zero #-}

  one = 1
  {-# INLINE one #-}


instance Elt RGB where
  touch (RGB r g b) = touch r >> touch g >> touch b
  {-# INLINE touch #-}
  
  zero             = 0
  {-# INLINE zero #-}

  one              = 1
  {-# INLINE one #-}


instance Elt HSI where
  touch (HSI h s i) = touch h >> touch s >> touch i
  {-# INLINE touch #-}
  
  zero             = 0
  {-# INLINE zero #-}

  one              = 1
  {-# INLINE one #-}


instance (Elt px, AlphaChannel px) => Elt (Alpha px) where
  touch (Alpha px a) = touch px >> touch a 
  {-# INLINE touch #-}
  
  zero             = 0
  {-# INLINE zero #-}

  one              = 1
  {-# INLINE one #-}
  

instance (Elt px, ComplexChannel px) => Elt (Complex px) where
  touch (x :+: y) = touch x >> touch y
  {-# INLINE touch #-}
  
  zero = 0
  {-# INLINE zero #-}
  
  one = 1
  {-# INLINE one #-}
  


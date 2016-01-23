{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Graphics.Image.Repa.Pixel (
  Pixel,
  -- * Binary
  Binary, on, off, isOn, isOff,
  -- * Grayscale and Colors
  Gray(..), RGB(..), HSI(..),
  -- * Complex
  Complex(..), ComplexPixel,
  -- * Alpha
  Alpha(..), AlphaPixel, fmapAlpha, combineAlpha
  ) where

import Data.Array.Repa.Eval (Elt(..))
import HIP.Pixel hiding (Pixel, ComplexPixel, AlphaPixel)
import qualified Graphics.Image.Pixel as U (Pixel, ComplexPixel, AlphaPixel)


-- | Repa can only work with 'I.Pixel's that implement 'Elt' and 'Unbox'
class (Elt px, U.Pixel px) => Pixel px where


-- | Repa can only work with 'I.Pixel's that implement 'Elt' and 'Unbox'
class (Elt px, Elt (Channel px), U.ComplexPixel px) =>
      ComplexPixel px where

-- | Repa can only work with 'I.Pixel's that implement 'Elt' and 'Unbox'
class (Elt px, Elt (Channel px), U.AlphaPixel px) =>
      AlphaPixel px where
        

-- | Repa Pixel  
instance Pixel Binary where

-- | Repa Pixel  
instance Pixel Gray where

-- | Repa Pixel  
instance Pixel RGB where

-- | Repa Pixel  
instance Pixel HSI where
 
-- | Repa Pixel  
instance ComplexPixel px => Pixel (Complex px) where

-- | Repa Pixel  
instance AlphaPixel px => Pixel (Alpha px) where
  

-- | Repa Pixel  
instance ComplexPixel Gray where

-- | Repa Pixel  
instance ComplexPixel RGB where

-- | Repa Pixel  
instance ComplexPixel HSI where

  
-- | Repa Pixel  
instance (Pixel (Alpha px), ComplexPixel px, AlphaPixel px) =>
         ComplexPixel (Alpha px) where
  

-- | Repa Pixel  
instance AlphaPixel Gray where

-- | Repa Pixel  
instance AlphaPixel RGB where

-- | Repa Pixel  
instance AlphaPixel HSI where


  
instance Elt Binary where
  touch (Binary y) = touch y
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


instance (Elt px, AlphaPixel px) => Elt (Alpha px) where
  touch (Alpha px a) = touch px >> touch a 
  {-# INLINE touch #-}
  
  zero             = 0
  {-# INLINE zero #-}

  one              = 1
  {-# INLINE one #-}
  

instance (Elt px, ComplexPixel px) => Elt (Complex px) where
  touch (x :+: y) = touch x >> touch y
  {-# INLINE touch #-}
  
  zero = 0
  {-# INLINE zero #-}
  
  one = 1
  {-# INLINE one #-}
  


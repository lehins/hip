{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Graphics.Image.Repa.Pixel (
  Pixel,
  -- * Binary
  Binary, on, off, isOn, isOff,
  -- * Grayscale and Colors
  Gray(..), RGB(..), HSI(..),
  -- * Complex
  Complex(..), ComplexInner,
  -- * Alpha
  Alpha(..), AlphaInner, fmapAlpha, combineAlpha
  ) where

import Data.Array.Repa.Eval (Elt(..))
import Graphics.Image.Interface.Pixel hiding (Pixel, ComplexInner, AlphaInner)
import qualified Graphics.Image.Unboxed.Pixel as U (Pixel, ComplexInner, AlphaInner)


-- | Repa can only work with 'I.Pixel's that implement 'Elt' and 'Unbox'
class (Elt px, U.Pixel px) => Pixel px where


-- | Repa can only work with 'I.Pixel's that implement 'Elt' and 'Unbox'
class (Elt px, Elt (Inner px), U.ComplexInner px) =>
      ComplexInner px where

-- | Repa can only work with 'I.Pixel's that implement 'Elt' and 'Unbox'
class (Elt px, Elt (Inner px), U.AlphaInner px) =>
      AlphaInner px where
        

-- | Repa Pixel  
instance Pixel Binary where

-- | Repa Pixel  
instance Pixel Gray where

-- | Repa Pixel  
instance Pixel RGB where

-- | Repa Pixel  
instance Pixel HSI where
 
-- | Repa Pixel  
instance ComplexInner px => Pixel (Complex px) where

-- | Repa Pixel  
instance AlphaInner px => Pixel (Alpha px) where
  

-- | Repa Pixel  
instance ComplexInner Gray where

-- | Repa Pixel  
instance ComplexInner RGB where

-- | Repa Pixel  
instance ComplexInner HSI where

  
-- | Repa Pixel  
instance (Pixel (Alpha px), ComplexInner px, AlphaInner px) =>
         ComplexInner (Alpha px) where
  

-- | Repa Pixel  
instance AlphaInner Gray where

-- | Repa Pixel  
instance AlphaInner RGB where

-- | Repa Pixel  
instance AlphaInner HSI where


  
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


instance (Elt px, AlphaInner px) => Elt (Alpha px) where
  touch (Alpha px a) = touch px >> touch a 
  {-# INLINE touch #-}
  
  zero             = 0
  {-# INLINE zero #-}

  one              = 1
  {-# INLINE one #-}
  

instance (Elt px, ComplexInner px) => Elt (Complex px) where
  touch (x :+: y) = touch x >> touch y
  {-# INLINE touch #-}
  
  zero = 0
  {-# INLINE zero #-}
  
  one = 1
  {-# INLINE one #-}
  


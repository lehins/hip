{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Graphics.Image.Repa.Pixel (
  Pixel,
  -- * Binary
  Binary, on, off, isOn, isOff,
  -- * Grayscale and Colors
  Gray(..), RGB(..), HSI(..),
  -- * Complex
  Complex(..), RealPixel,
  -- * Alpha
  Alpha(..), OpaquePixel, fmapAlpha, combineAlpha
  ) where

import Data.Array.Repa.Eval (Elt(..))
import HIP.Pixel hiding (Pixel, RealPixel, OpaquePixel)
import qualified Graphics.Image.Pixel as U (Pixel, RealPixel, OpaquePixel)


-- | Repa can only work with 'I.Pixel's that implement 'Elt' and 'Unbox'
class (Elt px, U.Pixel px) => Pixel px where


-- | Repa can only work with 'I.Pixel's that implement 'Elt' and 'Unbox'
class (Elt px, Elt (Channel px), U.RealPixel px) =>
      RealPixel px where

-- | Repa can only work with 'I.Pixel's that implement 'Elt' and 'Unbox'
class (Elt px, Elt (Channel px), U.OpaquePixel px) =>
      OpaquePixel px where
        

-- | Repa Pixel  
instance Pixel Binary where

-- | Repa Pixel  
instance Pixel Gray where

-- | Repa Pixel  
instance Pixel RGB where

-- | Repa Pixel  
instance Pixel HSI where
 
-- | Repa Pixel  
instance RealPixel px => Pixel (Complex px) where

-- | Repa Pixel  
instance OpaquePixel px => Pixel (Alpha px) where
  

-- | Repa Pixel  
instance RealPixel Gray where

-- | Repa Pixel  
instance RealPixel RGB where

-- | Repa Pixel  
instance RealPixel HSI where

  
-- | Repa Pixel  
instance (Pixel (Alpha px), RealPixel px, OpaquePixel px) =>
         RealPixel (Alpha px) where
  

-- | Repa Pixel  
instance OpaquePixel Gray where

-- | Repa Pixel  
instance OpaquePixel RGB where

-- | Repa Pixel  
instance OpaquePixel HSI where


  
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


instance (Elt px, OpaquePixel px) => Elt (Alpha px) where
  touch (Alpha px a) = touch px >> touch a 
  {-# INLINE touch #-}
  
  zero             = 0
  {-# INLINE zero #-}

  one              = 1
  {-# INLINE one #-}
  

instance (Elt px, RealPixel px) => Elt (Complex px) where
  touch (x :+ y) = touch x >> touch y
  {-# INLINE touch #-}
  
  zero = 0
  {-# INLINE zero #-}
  
  one = 1
  {-# INLINE one #-}
  


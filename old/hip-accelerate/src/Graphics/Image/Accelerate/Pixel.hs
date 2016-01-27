{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Graphics.Image.Accelerate.Pixel (
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

import Data.Array.Accelerate (Lift(..))
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar
import HIP.Pixel hiding (Pixel, RealPixel, OpaquePixel)
import qualified HIP.Pixel as I (Pixel, RealPixel, OpaquePixel)

 
-- | Accelerate can only work with 'I.Pixel's that implement 'Elt'
class (Elt (Channel px), Elt px, IsNum px, I.Pixel px) => Pixel px where


instance (Ord (Channel (Exp px)), IsNum px, Pixel px) => I.Pixel (Exp px) where
  type Channel (Exp px) = Exp (Channel px)

  fromDouble = constant . fromDouble

  fromChannel = fromChannel

  
-- | Accelerate can only work with 'I.Pixel's that implement 'Elt'
class (Pixel px, I.RealPixel px) => RealPixel px where

        
-- | Accelerate can only work with 'I.Pixel's that implement 'Elt'
class (Pixel px, I.OpaquePixel px) => OpaquePixel px where
        
{-
 
-- | Accelerate Pixel  
instance Pixel Binary where

-- | Accelerate Pixel  
instance Pixel Gray where

-- | Accelerate Pixel  
instance Pixel RGB where

-- | Accelerate Pixel  
instance Pixel HSI where


-- | Accelerate Pixel  
instance RealPixel px => Pixel (Complex px) where

-- | Accelerate Pixel  
instance OpaquePixel px => Pixel (Alpha px) where
  

-- | Accelerate Pixel  
instance RealPixel Gray where

-- | Accelerate Pixel  
instance RealPixel RGB where

-- | Accelerate Pixel  
instance RealPixel HSI where

  
-- | Accelerate Pixel  
instance (Pixel (Alpha px), RealPixel px, OpaquePixel px) =>
         RealPixel (Alpha px) where
  

-- | Accelerate Pixel  
instance OpaquePixel Gray where

-- | Accelerate Pixel  
instance OpaquePixel RGB where

-- | Accelerate Pixel  
instance OpaquePixel HSI where
-}

instance Lift Exp Binary where
  type Plain Binary = Binary
  lift = constant


instance Lift Exp Gray where
  type Plain Gray = Gray
  lift = constant


instance Lift Exp RGB where
  type Plain RGB = RGB
  lift = constant

  
instance Lift Exp HSI where
  type Plain HSI = HSI
  lift = constant
  
  
type instance EltRepr Binary = ((), Bool)
type instance EltRepr Gray = ((), Double)
type instance EltRepr RGB = (EltRepr (Double, Double), EltRepr' Double)
type instance EltRepr HSI = (EltRepr (Double, Double), EltRepr' Double)
--type instance EltRepr (Alpha px) = (EltRepr px, EltRepr' px)
type instance EltRepr (Complex px) = (EltRepr px, EltRepr' px)

type instance EltRepr' Binary = Bool
type instance EltRepr' Gray = Double
type instance EltRepr' RGB = (EltRepr (Double, Double), EltRepr' Double)
type instance EltRepr' HSI = (EltRepr (Double, Double), EltRepr' Double)
--type instance EltRepr (Alpha px) = (EltRepr px, EltRepr' px)
type instance EltRepr (Complex px) = (EltRepr px, EltRepr' px)
  
instance Elt Binary where
  eltType _           = eltType (undefined :: Channel Binary)
  toElt ((), b)       = Binary b
  fromElt (Binary b)  = ((), b)

  eltType' _          = eltType' (undefined :: Channel Binary)
  toElt'              = Binary
  fromElt' (Binary b) = b


instance Elt Gray where
  eltType _         = eltType (undefined :: Channel Gray)
  toElt ((), y)     = Gray y
  fromElt (Gray y)  = ((), y)

  eltType' _        = eltType' (undefined :: Channel Gray)
  toElt'            = Gray
  fromElt' (Gray y) = y
  

instance Elt RGB where 
  eltType _                =  PairTuple (eltType (undefined :: (Channel RGB, Channel RGB)))
                                        (eltType' (undefined :: Channel RGB))
  toElt ((((), r), g), b)  = RGB r g b
  fromElt (RGB r g b)      = ((((), r), g), b)

  eltType' _               = PairTuple (eltType (undefined :: (Channel RGB, Channel RGB)))
                                       (eltType' (undefined :: Channel RGB))
  toElt' ((((), r), g), b) = RGB r g b
  fromElt' (RGB r g b)     = ((((), r), g), b)


instance Elt HSI where
  eltType _                =  PairTuple (eltType (undefined :: (Channel HSI, Channel HSI)))
                                        (eltType' (undefined :: Channel HSI))
  toElt ((((), h), s), i)  = HSI h s i
  fromElt (HSI h s i)      = ((((), h), s), i)

  eltType' _               = PairTuple (eltType (undefined :: (Channel HSI, Channel HSI)))
                                       (eltType' (undefined :: Channel HSI))
  toElt' ((((), h), s), i) = HSI h s i
  fromElt' (HSI h s i)     = ((((), h), s), i)


--instance (Pixel px, OpaquePixel px) => Elt (Alpha px) where
  

--instance (Elt px, RealPixel px) => Elt (Complex px) where
  


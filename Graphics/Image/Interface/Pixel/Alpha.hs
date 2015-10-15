{-# LANGUAGE BangPatterns, FlexibleContexts, GADTs, TypeFamilies, UndecidableInstances #-}
module Graphics.Image.Interface.Pixel.Alpha (
  Alpha(..), AlphaInner, fmapAlpha, combineAlpha
  ) where

import Graphics.Image.Interface (Pixel(..))


{- | Every instance of this ComplexInner class can be used as a real and imaginary
parts of a Complex pixel. -}
class (Floating (Inner px), Fractional (Inner px), 
       Floating px, Fractional px, Ord px, Pixel px) =>
      AlphaInner px where

        
data Alpha px where
  A :: AlphaInner px => px -> (Inner px) -> Alpha px 


{- | Although 'Alpha' pixels are also 'Num', 'Floating' and 'Fractional' all of the
numeric operators affect only the underlying pixel values not the alpha
channels. In all operations of arity two first pixel has the precedence, hence
it's alpha channel is retained and the other one's is dropped.

>>> A (Gray 0.5) 0.7 + A (Gray 0.1) 0.8
<Alpha:(<Gray:(0.6)>|0.7)>

-}
instance AlphaInner px => Pixel (Alpha px) where
  type Inner (Alpha px) = Inner px

  pixel v = A (pixel v) 1
  
  pxOp op (A px a) = A (pxOp op px) a

  pxOp2 op (A px1 a1) (A px2 _) = (A (pxOp2 op px1 px2) a1)

  strongest (A px a) = A (strongest px) a

  weakest (A px a) = A (weakest px) a

  showType (A px _) = (showType px)++"A"


instance AlphaInner px => Num (Alpha px) where
  (+)           = pxOp2 (+)
  {-# INLINE (+) #-}
  
  (-)           = pxOp2 (-)
  {-# INLINE (-) #-}
  
  (*)           = pxOp2 (*)
  {-# INLINE (*) #-}
  
  abs           = pxOp abs
  {-# INLINE abs #-}
  
  signum        = pxOp signum
  {-# INLINE signum #-}
  
  fromInteger n = pixel . fromIntegral $ n
  {-# INLINE fromInteger #-}


instance AlphaInner px => Fractional (Alpha px) where
  (/)            = pxOp2 (/)
  recip          = pxOp recip
  fromRational n = pixel . fromRational $ n


instance AlphaInner px => Floating (Alpha px) where
  {-# INLINE pi #-}
  pi      = pixel pi
  {-# INLINE exp #-}
  exp     = pxOp exp
  {-# INLINE log #-}
  log     = pxOp log
  {-# INLINE sin #-}
  sin     = pxOp sin
  {-# INLINE cos #-}
  cos     = pxOp cos
  {-# INLINE asin #-}
  asin    = pxOp asin
  {-# INLINE atan #-}
  atan    = pxOp atan
  {-# INLINE acos #-}
  acos    = pxOp acos
  {-# INLINE sinh #-}
  sinh    = pxOp sinh
  {-# INLINE cosh #-}
  cosh    = pxOp cosh
  {-# INLINE asinh #-}
  asinh   = pxOp asinh
  {-# INLINE atanh #-}
  atanh   = pxOp atanh
  {-# INLINE acosh #-}
  acosh   = pxOp acosh


instance (AlphaInner px) => Eq (Alpha px) where
  (==) !(A px1 a1) !(A px2 a2) = px1 == px2 && a1 == a2


instance (AlphaInner px) => Ord (Alpha px) where
  compare (A px1 a1) (A px2 a2) | px1 < px2 = LT
                                | px1 > px2 = GT
                                | a1 < a2   = LT
                                | a1 > a2   = GT
                                | otherwise = EQ
  {-# INLINE (<=) #-}


instance AlphaInner px => Show (Alpha px) where
  {-# INLINE show #-}
  show (A px a) = "<Alpha:("++show px++"|"++show a++")>"


fmapAlpha :: AlphaInner px =>
             (Inner px -> Inner px)
             -> Alpha px
             -> Alpha px
fmapAlpha op (A px a) = A px (op a)
{-# INLINE fmapAlpha #-}


{- | Combines two pixels with 'Alpha' channels in a way specified by input functions. -}
combineAlpha :: AlphaInner px =>
                (px -> px -> px)
                -- ^ Function that combines the actual pixel values.
             -> (Inner px -> Inner px -> Inner px)
                -- ^ Function that combines the alpha channels for two pixel.
             -> Alpha px -- ^ First pixel
             -> Alpha px -- ^ Second Pixel
             -> Alpha px
combineAlpha pxOp2' aOp2 (A px1 a1) (A px2 a2) = A (pxOp2' px1 px2) (aOp2 a1 a2)
{-# INLINE combineAlpha #-}



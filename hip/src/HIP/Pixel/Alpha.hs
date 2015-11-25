{-# LANGUAGE BangPatterns, FlexibleContexts, GADTs, TypeFamilies, UndecidableInstances #-}
module HIP.Pixel.Alpha (
  Alpha(..), addAlpha, dropAlpha, fmapAlpha, combineAlpha
  ) where

import HIP.Interface (Pixel(..))


{- | This pixel wraps other pixel types, effectively adding an opacity layer to
it. Although 'Alpha' pixels are also 'Num', 'Floating' and 'Fractional' all of
the numeric operators affect only the underlying pixel values not the alpha
channels. In all operations of arity two first pixel has the precedence, hence
it's alpha channel is retained and the other one's is dropped. Use 'combineAlpha'
to address that issue.

>>> Alpha 0.8 (Gray 0.5) + Alpha 0.2 (Gray 0.1)
<Alpha:(0.8|<Gray:(0.6)>)>

-}
data Alpha px where
  Alpha :: Pixel px => (Inner px) -> px -> Alpha px
  

instance Pixel px => Pixel (Alpha px) where
  type Inner (Alpha px) = Inner px

  pixel = Alpha 1 . pixel
  {-# INLINE pixel #-}
  
  arity (Alpha _ px) = 1 + arity px
  {-# INLINE arity #-}

  ref 0 (Alpha a _) = a
  ref n (Alpha _ px) = ref (n-1) px
  {-# INLINE ref #-}

  apply !(f0:rest) !(Alpha a px) = Alpha (f0 a) $ apply rest px
  apply _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply #-}

  apply2 !(f0:rest) !(Alpha a1 px1) !(Alpha a2 px2) = Alpha (f0 a1 a2) (apply2 rest px1 px2)
  apply2 _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
  {-# INLINE apply2 #-}

  showType (Alpha _ px) = (showType px)++"-A"


instance Pixel px => Num (Alpha px) where
  (+) !(Alpha a1 px1) !(Alpha a2 px2) = Alpha (a1 + a2) (px1 + px2)
  {-# INLINE (+) #-}
  
  (-) !(Alpha a1 px1) !(Alpha a2 px2) = Alpha (a1 - a2) (px1 - px2)
  {-# INLINE (-) #-}
  
  (*) !(Alpha a1 px1) !(Alpha a2 px2) = Alpha (a1 * a2) (px1 * px2)
  {-# INLINE (*) #-}
  
  abs !(Alpha a px)                   = Alpha (abs a) (abs px)
  {-# INLINE abs #-}
  
  signum !(Alpha a px)                = Alpha (signum a) (signum px)
  {-# INLINE signum #-}
  
  fromInteger                         = pixel . fromIntegral 
  {-# INLINE fromInteger #-}


instance (Pixel px, Fractional px, Fractional (Inner px)) => Fractional (Alpha px) where
  (/) !(Alpha a1 px1) !(Alpha a2 px2) = Alpha (a1 / a2) (px1 / px2)
  {-# INLINE (/) #-}
  
  recip !(Alpha a px)                 = Alpha (recip a) (recip px)
  {-# INLINE recip #-}
  
  fromRational                        = pixel . fromRational 
  {-# INLINE fromRational #-}


instance (Pixel px, Floating px, Floating (Inner px)) => Floating (Alpha px) where
  pi                   = pixel pi
  {-# INLINE pi #-}
  
  exp !(Alpha a px)    = Alpha (exp a) (exp px)
  {-# INLINE exp #-}
  
  log !(Alpha a px)    = Alpha (log a) (log px)
  {-# INLINE log #-}
  
  sin !(Alpha a px)    = Alpha (sin a) (sin px)
  {-# INLINE sin #-}
  
  cos !(Alpha a px)    = Alpha (cos a) (cos px)
  {-# INLINE cos #-}
  
  asin !(Alpha a px)   = Alpha (asin a) (asin px)
  {-# INLINE asin #-}
  
  atan !(Alpha a px)   = Alpha (atan a) (atan px)
  {-# INLINE atan #-}
  
  acos !(Alpha a px)   = Alpha (acos a) (acos px)
  {-# INLINE acos #-}
  
  sinh !(Alpha a px)   = Alpha (sinh a) (sinh px)
  {-# INLINE sinh #-}
  
  cosh !(Alpha a px)   = Alpha (cosh a) (cosh px)
  {-# INLINE cosh #-}
  
  asinh !(Alpha a px)  = Alpha (asinh a) (asinh px)
  {-# INLINE asinh #-}
  
  atanh !(Alpha a px)  = Alpha (atanh a) (atanh px)
  {-# INLINE atanh #-}
  
  acosh !(Alpha a px)  = Alpha (acosh a) (acosh px)
  {-# INLINE acosh #-}


instance Pixel px => Eq (Alpha px) where
  (==) !(Alpha a1 px1) !(Alpha a2 px2) = px1 == px2 && a1 == a2
  {-# INLINE (==) #-}


instance (Pixel px, Ord px) => Ord (Alpha px) where
  compare !(Alpha a1 px1) !(Alpha a2 px2) | px1 < px2 = LT
                                          | px1 > px2 = GT
                                          | a1 < a2   = LT
                                          | a1 > a2   = GT
                                          | otherwise = EQ
  {-# INLINE compare #-}


instance Pixel px => Show (Alpha px) where
  show (Alpha a px) = "<Alpha:("++show a++"|"++show px++")>"


-- | Add an Alpha channel to a pixel.
--
-- >>> addAlpha $ RGB 0.1 0.3 0.5
-- <Alpha:(1.0|<RGB:(0.1|0.3|0.5)>)>
--
addAlpha :: Pixel px =>
            px
         -> Alpha px
addAlpha !px = Alpha 1 px
{-# INLINE addAlpha #-}


-- | Remove alpha channel from a pixel.
--
-- >>> dropAlpha $ Alpha 0.2 (RGB 0.1 0.3 0.5)
-- <RGB:(0.1|0.3|0.5)>
--
dropAlpha :: Pixel px =>
             Alpha px
          -> px
dropAlpha !(Alpha _ px) = px
{-# INLINE dropAlpha #-}


-- | Apply a function to an alpha value.
--
-- >>> fmapAlpha (*2) $ Alpha 0.2 (Gray 0.5)
-- <Alpha:(0.4|<Gray:(0.5)>)>
--
fmapAlpha :: Pixel px =>
             (Inner px -> Inner px)
          -> Alpha px
          -> Alpha px
fmapAlpha !f !(Alpha a px) = Alpha (f a) px
{-# INLINE fmapAlpha #-}


-- | Combines two pixels with 'Alpha' channels in a way specified by input functions.
--
-- >>> combineAlpha (/) (+) (Alpha 0.8 (Gray 0.5)) (Alpha 0.2 (Gray 0.1))
-- <Alpha:(4.0|<Gray:(0.6)>)>
--
combineAlpha :: Pixel px =>
                (Inner px -> Inner px -> Inner px)
                -- ^ Function that combines the alpha channels for two pixel.
             -> (px -> px -> px)
                -- ^ Function that combixnes the actual pixel values.
             -> Alpha px -- ^ First pixel
             -> Alpha px -- ^ Second Pixel
             -> Alpha px
combineAlpha !aOp2 !pxOp2' !(Alpha a1 px1) !(Alpha a2 px2) = Alpha (aOp2 a1 a2) (pxOp2' px1 px2)
{-# INLINE combineAlpha #-}



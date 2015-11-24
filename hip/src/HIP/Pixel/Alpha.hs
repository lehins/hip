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
  
  pxOp !op !(Alpha a px) = Alpha a (pxOp op px)
  {-# INLINE pxOp #-}

  pxOp2 !op !(Alpha a1 px1 ) (Alpha _ px2) = Alpha a1 (pxOp2 op px1 px2) 
  {-# INLINE pxOp2 #-}

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

  strongest !(Alpha a px) = Alpha a (strongest px)
  {-# INLINE strongest #-}

  weakest !(Alpha a px) = Alpha a (weakest px)
  {-# INLINE weakest #-}

  showType (Alpha _ px) = (showType px)++"-A"


instance Pixel px => Num (Alpha px) where
  (+)         = pxOp2 (+)
  {-# INLINE (+) #-}
  
  (-)         = pxOp2 (-)
  {-# INLINE (-) #-}
  
  (*)         = pxOp2 (*)
  {-# INLINE (*) #-}
  
  abs         = pxOp abs
  {-# INLINE abs #-}
  
  signum      = pxOp signum
  {-# INLINE signum #-}
  
  fromInteger = pixel . fromIntegral 
  {-# INLINE fromInteger #-}


instance (Pixel px, Fractional px, Fractional (Inner px)) => Fractional (Alpha px) where
  (/)          = pxOp2 (/)
  {-# INLINE (/) #-}
  
  recip        = pxOp recip
  {-# INLINE recip #-}
  
  fromRational = pixel . fromRational 
  {-# INLINE fromRational #-}


instance (Pixel px, Floating px, Floating (Inner px)) => Floating (Alpha px) where
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



{-# LANGUAGE BangPatterns, FlexibleContexts, GADTs, TypeFamilies, UndecidableInstances #-}
module Graphics.Image.Interface.Pixel.Alpha (
  Alpha(..), AlphaInner, dropAlpha, fmapAlpha, combineAlpha
  ) where

import Graphics.Image.Interface (Pixel(..))


{- | Every instance of this 'AlphaInner' class can be used as internal pixel. -}
class (Floating (Inner px), Fractional (Inner px), 
       Floating px, Fractional px, Ord px, Pixel px
      ) => AlphaInner px where

        
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
  Alpha :: AlphaInner px => (Inner px) -> px -> Alpha px 


instance AlphaInner px => Pixel (Alpha px) where
  type Inner (Alpha px) = Inner px

  pixel !v = Alpha 1 (pixel v)
  {-# INLINE pixel #-}
  
  pxOp !op !(Alpha a px) = Alpha a (pxOp op px)
  {-# INLINE pxOp #-}

  pxOp2 !op !(Alpha a1 px1 ) (Alpha _ px2) = Alpha a1 (pxOp2 op px1 px2) 
  {-# INLINE pxOp2 #-}

  strongest !(Alpha a px) = Alpha a (strongest px)
  {-# INLINE strongest #-}

  weakest !(Alpha a px) = Alpha a (weakest px)
  {-# INLINE weakest #-}

  showType (Alpha _ px) = (showType px)++"A"


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
  {-# INLINE (/) #-}
  
  recip          = pxOp recip
  {-# INLINE recip #-}
  
  fromRational !n = pixel . fromRational $ n
  {-# INLINE fromRational #-}


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
  (==) !(Alpha a1 px1) !(Alpha a2 px2) = px1 == px2 && a1 == a2
  {-# INLINE (==) #-}


instance (AlphaInner px) => Ord (Alpha px) where
  compare !(Alpha a1 px1) !(Alpha a2 px2) | px1 < px2 = LT
                                          | px1 > px2 = GT
                                          | a1 < a2   = LT
                                          | a1 > a2   = GT
                                          | otherwise = EQ
  {-# INLINE compare #-}


instance AlphaInner px => Show (Alpha px) where
  show (Alpha a px) = "<Alpha:("++show a++"|"++show px++")>"

-- | Remove alpha channel from the pixel.
--
-- >>> dropAlpha $ Alpha 0.2 (RGB 0.1 0.3 0.5)
-- <RGB:(0.1|0.3|0.5)>
--
dropAlpha :: AlphaInner px =>
             Alpha px
             -> px
dropAlpha (Alpha _ px) = px
{-# INLINE dropAlpha #-}

-- | Apply a function to an alpha value.
--
-- >>> fmapAlpha (*2) $ Alpha 0.2 (Gray 0.5)
-- <Alpha:(0.4|<Gray:(0.5)>)>
--
fmapAlpha :: AlphaInner px =>
             (Inner px -> Inner px)
             -> Alpha px
             -> Alpha px
fmapAlpha f (Alpha a px) = Alpha (f a) px
{-# INLINE fmapAlpha #-}


-- | Combines two pixels with 'Alpha' channels in a way specified by input functions.
--
-- >>> combineAlpha (/) (+) (Alpha 0.8 (Gray 0.5)) (Alpha 0.2 (Gray 0.1))
-- <Alpha:(4.0|<Gray:(0.6)>)>
--
combineAlpha :: AlphaInner px =>
             (Inner px -> Inner px -> Inner px)
                -- ^ Function that combines the alpha channels for two pixel.
             -> (px -> px -> px)
                -- ^ Function that combixnes the actual pixel values.
             -> Alpha px -- ^ First pixel
             -> Alpha px -- ^ Second Pixel
             -> Alpha px
combineAlpha aOp2 pxOp2' (Alpha a1 px1) (Alpha a2 px2) = Alpha (aOp2 a1 a2) (pxOp2' px1 px2)
{-# INLINE combineAlpha #-}



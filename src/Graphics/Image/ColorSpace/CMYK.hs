{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Image.ColorSpace.CMYK
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.ColorSpace.CMYK (
  CMYK(..), CMYKA(..), Pixel(..), 
  ToCMYK(..), ToCMYKA(..)
  ) where

import Prelude hiding (map)
import Control.Applicative
import Data.Foldable
import Data.Typeable (Typeable)
import Foreign.Ptr
import Foreign.Storable

import Graphics.Image.Interface

------------
--- CMYK ---
------------

-- | Cyan, Magenta, Yellow and Black color space.
data CMYK = CyanCMYK -- ^ Cyan
          | MagCMYK  -- ^ Magenta
          | YelCMYK  -- ^ Yellow
          | KeyCMYK  -- ^ Key (Black)
          deriving (Eq, Enum, Show, Bounded, Typeable)


instance Show e => Show (Pixel CMYK e) where
  show (PixelCMYK c m y k) = "<CMYK:("++show c++"|"++show m++"|"++show y++"|"++show k++")>"


data instance Pixel CMYK e = PixelCMYK !e !e !e !e deriving Eq

instance (Elevator e, Typeable e) => ColorSpace CMYK e where
  type Components CMYK e = (e, e, e, e)
  
  fromComponents !(c, m, y, k) = PixelCMYK c m y k
  {-# INLINE fromComponents #-}
  toComponents (PixelCMYK c m y k) = (c, m, y, k)
  {-# INLINE toComponents #-}
  promote !e = PixelCMYK e e e e
  {-# INLINE promote #-}
  getPxC (PixelCMYK c _ _ _) CyanCMYK = c
  getPxC (PixelCMYK _ m _ _) MagCMYK  = m
  getPxC (PixelCMYK _ _ y _) YelCMYK  = y
  getPxC (PixelCMYK _ _ _ k) KeyCMYK  = k
  {-# INLINE setPxC #-}
  setPxC (PixelCMYK _ m y k) CyanCMYK c = PixelCMYK c m y k
  setPxC (PixelCMYK c _ y k) MagCMYK  m = PixelCMYK c m y k
  setPxC (PixelCMYK c m _ k) YelCMYK  y = PixelCMYK c m y k
  setPxC (PixelCMYK c m y _) KeyCMYK  k = PixelCMYK c m y k
  {-# INLINE getPxC #-}
  mapPxC f (PixelCMYK c m y k) =
    PixelCMYK (f CyanCMYK c) (f MagCMYK m) (f YelCMYK y) (f KeyCMYK k)
  {-# INLINE mapPxC #-}
  liftPx = fmap
  {-# INLINE liftPx #-}
  liftPx2 = liftA2
  {-# INLINE liftPx2 #-}
  foldlPx = foldl'
  {-# INLINE foldlPx #-}
  foldlPx2 f !z (PixelCMYK c1 m1 y1 k1) (PixelCMYK c2 m2 y2 k2) =
    f (f (f (f z c1 c2) m1 m2) y1 y2) k1 k2
  {-# INLINE foldlPx2 #-}


instance Functor (Pixel CMYK) where
  fmap f (PixelCMYK c m y k) = PixelCMYK (f c) (f m) (f y) (f k)
  {-# INLINE fmap #-}


instance Applicative (Pixel CMYK) where
  pure !e = PixelCMYK e e e e
  {-# INLINE pure #-}
  (PixelCMYK fc fm fy fk) <*> (PixelCMYK c m y k) = PixelCMYK (fc c) (fm m) (fy y) (fk k)
  {-# INLINE (<*>) #-}


instance Foldable (Pixel CMYK) where
  foldr f !z (PixelCMYK c m y k) = f c (f m (f y (f k z)))
  {-# INLINE foldr #-}


instance Num e => Num (Pixel CMYK e) where
  (+)         = liftA2 (+)
  {-# INLINE (+) #-}
  (-)         = liftA2 (-)
  {-# INLINE (-) #-}
  (*)         = liftA2 (*)
  {-# INLINE (*) #-}
  abs         = liftA abs
  {-# INLINE abs #-}
  signum      = liftA signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}


instance Fractional e => Fractional (Pixel CMYK e) where
  (/)          = liftA2 (/)
  {-# INLINE (/) #-}
  recip        = liftA recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}


instance Floating e => Floating (Pixel CMYK e) where
  pi      = pure pi
  {-# INLINE pi #-}
  exp     = liftA exp
  {-# INLINE exp #-}
  log     = liftA log
  {-# INLINE log #-}
  sin     = liftA sin
  {-# INLINE sin #-}
  cos     = liftA cos
  {-# INLINE cos #-}
  asin    = liftA asin
  {-# INLINE asin #-}
  atan    = liftA atan
  {-# INLINE atan #-}
  acos    = liftA acos
  {-# INLINE acos #-}
  sinh    = liftA sinh
  {-# INLINE sinh #-}
  cosh    = liftA cosh
  {-# INLINE cosh #-}
  asinh   = liftA asinh
  {-# INLINE asinh #-}
  atanh   = liftA atanh
  {-# INLINE atanh #-}
  acosh   = liftA acosh
  {-# INLINE acosh #-}


instance Storable e => Storable (Pixel CMYK e) where

  sizeOf _ = 3 * sizeOf (undefined :: e)
  alignment _ = alignment (undefined :: e)
  peek p = do
    q <- return $ castPtr p
    c <- peek q
    m <- peekElemOff q 1
    y <- peekElemOff q 2
    k <- peekElemOff q 3
    return (PixelCMYK c m y k)
  poke p (PixelCMYK c m y k) = do
    q <- return $ castPtr p
    poke q c
    pokeElemOff q 1 m
    pokeElemOff q 2 y
    pokeElemOff q 3 k

-------------
--- CMYKA ---
-------------

-- | Cyan, Magenta, Yellow and Black color space with Alpha channel.
data CMYKA = CyanCMYKA  -- ^ Cyan
           | MagCMYKA   -- ^ Magenta
           | YelCMYKA   -- ^ Yellow
           | KeyCMYKA   -- ^ Key (Black)
           | AlphaCMYKA -- ^ Alpha 
           deriving (Eq, Enum, Show, Bounded, Typeable)


-- | Conversion to `CMYK` color space.
class ColorSpace cs Double => ToCMYK cs where

  -- | Convert to a `CMYK` pixel.
  toPixelCMYK :: Pixel cs Double -> Pixel CMYK Double

  -- | Convert to a `CMYK` image.
  toImageCMYK :: (Array arr cs Double, Array arr CMYK Double) =>
                 Image arr cs Double
              -> Image arr CMYK Double
  toImageCMYK = map toPixelCMYK
  {-# INLINE toImageCMYK #-}



-- | Conversion to `CMYKA` from another color space with Alpha channel.
class (ToCMYK (Opaque cs), AlphaSpace cs Double) => ToCMYKA cs where

  -- | Convert to a `CMYKA` pixel.
  toPixelCMYKA :: Pixel cs Double -> Pixel CMYKA Double
  toPixelCMYKA px = addAlpha (getAlpha px) (toPixelCMYK (dropAlpha px))
  {-# INLINE toPixelCMYKA #-}

  -- | Convert to a `CMYKA` image.
  toImageCMYKA :: (Array arr cs Double, Array arr CMYKA Double) =>
                  Image arr cs Double
               -> Image arr CMYKA Double
  toImageCMYKA = map toPixelCMYKA
  {-# INLINE toImageCMYKA #-}

data instance Pixel CMYKA e = PixelCMYKA !e !e !e !e !e deriving Eq


instance Show e => Show (Pixel CMYKA e) where
  show (PixelCMYKA c m y k a) =
    "<CMYKA:("++show c++"|"++show m++"|"++show y++"|"++show k++"|"++show a++")>"


instance (Elevator e, Typeable e) => ColorSpace CMYKA e where
  type Components CMYKA e = (e, e, e, e, e)
  
  fromComponents !(c, m, y, k, a) = PixelCMYKA c m y k a
  {-# INLINE fromComponents #-}
  toComponents (PixelCMYKA c m y k a) = (c, m, y, k, a)
  {-# INLINE toComponents #-}
  promote !e = PixelCMYKA e e e e e
  {-# INLINE promote #-}
  getPxC (PixelCMYKA c _ _ _ _) CyanCMYKA  = c
  getPxC (PixelCMYKA _ m _ _ _) MagCMYKA   = m
  getPxC (PixelCMYKA _ _ y _ _) YelCMYKA   = y
  getPxC (PixelCMYKA _ _ _ k _) KeyCMYKA   = k
  getPxC (PixelCMYKA _ _ _ _ a) AlphaCMYKA = a
  {-# INLINE getPxC #-}
  setPxC (PixelCMYKA _ m y k a) CyanCMYKA  c = PixelCMYKA c m y k a
  setPxC (PixelCMYKA c _ y k a) MagCMYKA   m = PixelCMYKA c m y k a
  setPxC (PixelCMYKA c m _ k a) YelCMYKA   y = PixelCMYKA c m y k a
  setPxC (PixelCMYKA c m y _ a) KeyCMYKA   k = PixelCMYKA c m y k a
  setPxC (PixelCMYKA c m y k _) AlphaCMYKA a = PixelCMYKA c m y k a
  {-# INLINE setPxC #-}
  mapPxC f (PixelCMYKA c m y k a) =
    PixelCMYKA (f CyanCMYKA c) (f MagCMYKA m) (f YelCMYKA y) (f KeyCMYKA k) (f AlphaCMYKA a)
  {-# INLINE mapPxC #-}
  liftPx = fmap
  {-# INLINE liftPx #-}
  liftPx2 = liftA2
  {-# INLINE liftPx2 #-}
  foldlPx = foldl'
  {-# INLINE foldlPx #-}
  foldlPx2 f !z (PixelCMYKA c1 m1 y1 k1 a1) (PixelCMYKA c2 m2 y2 k2 a2) =
    f (f (f (f (f z c1 c2) m1 m2) y1 y2) k1 k2) a1 a2
  {-# INLINE foldlPx2 #-}


instance (Elevator e, Typeable e) => AlphaSpace CMYKA e where
  type Opaque CMYKA = CMYK

  getAlpha (PixelCMYKA _ _ _ _ a) = a
  {-# INLINE getAlpha #-}
  
  addAlpha !a (PixelCMYK c m y k) = PixelCMYKA c m y k a
  {-# INLINE addAlpha #-}

  dropAlpha (PixelCMYKA c m y k _) = PixelCMYK c m y k
  {-# INLINE dropAlpha #-}


instance Functor (Pixel CMYKA) where
  fmap f (PixelCMYKA c m y k a) = PixelCMYKA (f c) (f m) (f y) (f k) (f a)
  {-# INLINE fmap #-}


instance Applicative (Pixel CMYKA) where
  pure !e = PixelCMYKA e e e e e
  {-# INLINE pure #-}
  (PixelCMYKA fc fm fy fk fa) <*> (PixelCMYKA c m y k a) =
    PixelCMYKA (fc c) (fm m) (fy y) (fk k) (fa a)
  {-# INLINE (<*>) #-}


instance Foldable (Pixel CMYKA) where
  foldr f !z (PixelCMYKA c m y k a) = f c (f m (f y (f k (f a z))))
  {-# INLINE foldr #-}


instance Num e => Num (Pixel CMYKA e) where
  (+)         = liftA2 (+)
  {-# INLINE (+) #-}
  (-)         = liftA2 (-)
  {-# INLINE (-) #-}
  (*)         = liftA2 (*)
  {-# INLINE (*) #-}
  abs         = liftA abs
  {-# INLINE abs #-}
  signum      = liftA signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}


instance Fractional e => Fractional (Pixel CMYKA e) where
  (/)          = liftA2 (/)
  {-# INLINE (/) #-}
  recip        = liftA recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}


instance Floating e => Floating (Pixel CMYKA e) where
  pi      = pure pi
  {-# INLINE pi #-}
  exp     = liftA exp
  {-# INLINE exp #-}
  log     = liftA log
  {-# INLINE log #-}
  sin     = liftA sin
  {-# INLINE sin #-}
  cos     = liftA cos
  {-# INLINE cos #-}
  asin    = liftA asin
  {-# INLINE asin #-}
  atan    = liftA atan
  {-# INLINE atan #-}
  acos    = liftA acos
  {-# INLINE acos #-}
  sinh    = liftA sinh
  {-# INLINE sinh #-}
  cosh    = liftA cosh
  {-# INLINE cosh #-}
  asinh   = liftA asinh
  {-# INLINE asinh #-}
  atanh   = liftA atanh
  {-# INLINE atanh #-}
  acosh   = liftA acosh
  {-# INLINE acosh #-}


instance Storable e => Storable (Pixel CMYKA e) where

  sizeOf _ = 3 * sizeOf (undefined :: e)
  alignment _ = alignment (undefined :: e)
  peek p = do
    q <- return $ castPtr p
    c <- peek q
    m <- peekElemOff q 1
    y <- peekElemOff q 2
    k <- peekElemOff q 3
    a <- peekElemOff q 4
    return (PixelCMYKA c m y k a)
  poke p (PixelCMYKA c m y k a) = do
    q <- return $ castPtr p
    poke q c
    pokeElemOff q 1 m
    pokeElemOff q 2 y
    pokeElemOff q 3 k
    pokeElemOff q 4 a

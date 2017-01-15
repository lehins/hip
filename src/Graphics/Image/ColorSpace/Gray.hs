{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Image.ColorSpace.Gray
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.ColorSpace.Gray (
  Gray(..), Pixel(..), toGrayImages, fromGrayImages
  ) where

import Prelude as P
import Control.Applicative
import Data.Foldable
import Data.Typeable (Typeable)
import Foreign.Ptr
import Foreign.Storable

import Graphics.Image.Interface as I

-- ^ This is a single channel colorspace, that is designed to separate Gray
-- level values from other types of colorspace, hence it is not convertible to
-- or from, but rather is here to allow operation on arbirtary single channel
-- images. If you are looking for a true grayscale colorspace
-- 'Graphics.Image.ColorSpace.Luma.Y' should be used instead.
data Gray = Gray deriving (Eq, Enum, Show, Typeable)


data instance Pixel Gray e = PixelGray !e deriving (Ord, Eq)


instance Show e => Show (Pixel Gray e) where
  show (PixelGray g) = "<Gray:("++show g++")>"


instance (Elevator e, Typeable e) => ColorSpace Gray e where
  type Components Gray e = e

  broadcastC = PixelGray
  {-# INLINE broadcastC #-}
  fromComponents = PixelGray
  {-# INLINE fromComponents #-}
  toComponents (PixelGray g) = g
  {-# INLINE toComponents #-}
  getPxC (PixelGray g) Gray = g
  {-# INLINE getPxC #-}
  setPxC (PixelGray _) Gray g = PixelGray g
  {-# INLINE setPxC #-}
  mapPxC f (PixelGray g) = PixelGray (f Gray g)
  {-# INLINE mapPxC #-}
  mapPx = fmap
  {-# INLINE mapPx #-}
  zipWithPx = liftA2
  {-# INLINE zipWithPx #-}
  foldlPx = foldl'
  {-# INLINE foldlPx #-}


instance Functor (Pixel Gray) where
  fmap f (PixelGray g) = PixelGray (f g)
  {-# INLINE fmap #-}


instance Applicative (Pixel Gray) where
  pure = PixelGray
  {-# INLINE pure #-}
  (PixelGray fg) <*> (PixelGray g) = PixelGray (fg g)
  {-# INLINE (<*>) #-}


instance Foldable (Pixel Gray) where
  foldr f !z (PixelGray g) = f g z
  {-# INLINE foldr #-}


instance Monad (Pixel Gray) where

  return = PixelGray
  {-# INLINE return #-}

  (>>=) (PixelGray g) f = f g
  {-# INLINE (>>=) #-}


instance Num e => Num (Pixel Gray e) where
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


instance Fractional e => Fractional (Pixel Gray e) where
  (/)          = liftA2 (/)
  {-# INLINE (/) #-}
  recip        = liftA recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}


instance Floating e => Floating (Pixel Gray e) where
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


instance Storable e => Storable (Pixel Gray e) where

  sizeOf _ = sizeOf (undefined :: e)
  alignment _ = alignment (undefined :: e)
  peek p = do
    q <- return $ castPtr p
    g <- peek q
    return (PixelGray g)
  poke p (PixelGray g) = do
    q <- return $ castPtr p
    poke q g

-- | Separate an image into a list of images with 'Gray' pixels containing every
-- channel from the source image.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> let [frog_red, frog_green, frog_blue] = toGrayImages frog
-- >>> writeImage "images/frog_red.png" $ toImageY frog_red
-- >>> writeImage "images/frog_green.jpg" $ toImageY frog_green
-- >>> writeImage "images/frog_blue.jpg" $ toImageY frog_blue
--
-- <<images/frog_red.jpg>> <<images/frog_green.jpg>> <<images/frog_blue.jpg>>
--
toGrayImages :: (Array arr cs e, Array arr Gray e) => Image arr cs e -> [Image arr Gray e]
toGrayImages !img = P.map getCh (enumFrom (toEnum 0)) where
  getCh !ch = I.map (PixelGray . (`getPxC` ch)) img
  {-# INLINE getCh #-}
{-# INLINE toGrayImages #-}


-- | Combine a list of images with 'Gray' pixels into an image of any color
-- space, by supplying an order of color space channels.
--
-- For example here is a frog with swapped 'BlueRGB' and 'GreenRGB' channels.
--
-- >>> writeImage "images/frog_rbg.jpg" $ fromGrayImages [frog_red, frog_green, frog_blue] [RedRGB, BlueRGB, GreenRGB]
--
-- <<images/frog.jpg>> <<images/frog_rbg.jpg>>
--
-- It is worth noting though, despite that separating image channels can be sometimes
-- pretty useful, the same effect as above can be achieved in a much simpler and
-- a more efficient way:
--
-- @ map (\(PixelRGB r g b) -> PixelRGB r b g) frog @
--
fromGrayImages :: forall arr cs e . (Array arr Gray e, Array arr cs e) =>
                  [Image arr Gray e] -> [cs] -> Image arr cs e
fromGrayImages = fromGrays 0 where
  updateCh ch px (PixelGray e) = setPxC px ch e
  {-# INLINE updateCh #-}
  fromGrays img []     _      = img
  fromGrays img _      []     = img
  fromGrays img (i:is) (c:cs) = fromGrays (I.zipWith (updateCh c) img i) is cs
  {-# INLINE fromGrays #-}
{-# INLINE fromGrayImages #-}

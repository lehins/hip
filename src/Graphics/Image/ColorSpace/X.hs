{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Image.ColorSpace.X
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.ColorSpace.X (
  X(..), Pixel(..), toImagesX, fromImagesX
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
data X = X deriving (Eq, Enum, Bounded, Show, Typeable)


newtype instance Pixel X e = PixelX e deriving (Ord, Eq)


instance Show e => Show (Pixel X e) where
  show (PixelX g) = "<X:("++show g++")>"


instance Elevator e => ColorSpace X e where
  type Components X e = e

  promote = PixelX
  {-# INLINE promote #-}
  fromComponents = PixelX
  {-# INLINE fromComponents #-}
  toComponents (PixelX g) = g
  {-# INLINE toComponents #-}
  getPxC (PixelX g) X = g
  {-# INLINE getPxC #-}
  setPxC (PixelX _) X g = PixelX g
  {-# INLINE setPxC #-}
  mapPxC f (PixelX g) = PixelX (f X g)
  {-# INLINE mapPxC #-}
  liftPx = fmap
  {-# INLINE liftPx #-}
  liftPx2 = liftA2
  {-# INLINE liftPx2 #-}
  foldlPx = foldl'
  {-# INLINE foldlPx #-}
  foldlPx2 f !z (PixelX g1) (PixelX g2) = f z g1 g2
  {-# INLINE foldlPx2 #-}


instance Functor (Pixel X) where
  fmap f (PixelX g) = PixelX (f g)
  {-# INLINE fmap #-}


instance Applicative (Pixel X) where
  pure = PixelX
  {-# INLINE pure #-}
  (PixelX fg) <*> (PixelX g) = PixelX (fg g)
  {-# INLINE (<*>) #-}


instance Foldable (Pixel X) where
  foldr f !z (PixelX g) = f g z
  {-# INLINE foldr #-}


instance Monad (Pixel X) where

  return = PixelX
  {-# INLINE return #-}

  (>>=) (PixelX g) f = f g
  {-# INLINE (>>=) #-}


instance Storable e => Storable (Pixel X e) where

  sizeOf _ = sizeOf (undefined :: e)
  alignment _ = alignment (undefined :: e)
  peek p = do
    q <- return $ castPtr p
    g <- peek q
    return (PixelX g)
  poke p (PixelX g) = do
    q <- return $ castPtr p
    poke q g

-- | Separate an image into a list of images with 'X' pixels containing every
-- channel from the source image.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> let [frog_red, frog_green, frog_blue] = toImagesX frog
-- >>> writeImage "images/frog_red.png" $ toImageY frog_red
-- >>> writeImage "images/frog_green.jpg" $ toImageY frog_green
-- >>> writeImage "images/frog_blue.jpg" $ toImageY frog_blue
--
-- <<images/frog_red.jpg>> <<images/frog_green.jpg>> <<images/frog_blue.jpg>>
--
toImagesX :: (Array arr cs e, Array arr X e) => Image arr cs e -> [Image arr X e]
toImagesX !img = P.map getCh (enumFrom minBound) where
  getCh !ch = I.map (PixelX . (`getPxC` ch)) img
  {-# INLINE getCh #-}
{-# INLINE toImagesX #-}


-- | Combine a list of images with 'X' pixels into an image of any color
-- space, by supplying an order of color space channels.
--
-- For example here is a frog with swapped 'BlueRGB' and 'GreenRGB' channels.
--
-- >>> writeImage "images/frog_rbg.jpg" $ fromImagesX [frog_red, frog_green, frog_blue] [RedRGB, BlueRGB, GreenRGB]
--
-- <<images/frog.jpg>> <<images/frog_rbg.jpg>>
--
-- It is worth noting though, despite that separating image channels can be
-- sometimes pretty useful, exactly the same effect as in example above can be
-- achieved in a much simpler and a more efficient way:
--
-- @ map (\(PixelRGB r g b) -> PixelRGB r b g) frog @
--
fromImagesX :: forall arr cs e . (Array arr X e, Array arr cs e) =>
                  [(cs, Image arr X e)] -> Image arr cs e
fromImagesX = fromXs 0 where
  updateCh ch px (PixelX e) = setPxC px ch e
  {-# INLINE updateCh #-}
  fromXs img []      = img
  fromXs img ((c, i):xs) = fromXs (I.zipWith (updateCh c) img i) xs
  {-# INLINE fromXs #-}
{-# INLINE fromImagesX #-}

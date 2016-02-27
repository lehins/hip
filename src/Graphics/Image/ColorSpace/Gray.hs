{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             ScopedTypeVariables, TypeFamilies #-}
module Graphics.Image.ColorSpace.Gray (
  Gray(..), Pixel(..), toGrayImages, fromGrayImages
  ) where

import Prelude hiding (map, zipWith)
import qualified Prelude as P (map)
import Graphics.Image.Interface
import Data.Typeable (Typeable)
import Data.Monoid (mappend, mempty)

-- ^ This is a signgle channel colorspace, that is designed to hold any channel
-- from any other colorspace, hence it is not convertible to and from, but
-- rather is here to allow separation of channels from other multichannel
-- colorspaces. If you are looking for a true grayscale colorspace
-- 'Graphics.Image.ColorSpace.Luma.Y' should be used instead.
data Gray = Gray deriving (Eq, Enum, Show, Typeable)


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
  getCh !ch = map (PixelGray . (`getPxCh` ch)) img
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
-- It is worth noting though, that separating image channels can be sometimes
-- pretty useful, the same effect as above can be achieved in a much simpler and
-- more efficient way:
--
-- @ map (\(PixelRGB r g b) -> PixelRGB r b g) frog @
--
fromGrayImages :: forall arr cs e . (Array arr Gray e, Array arr cs e) =>
                  [Image arr Gray e] -> [cs] -> Image arr cs e
fromGrayImages imgs chs =
  fromGrays (singleton (fromChannel 0)) imgs chs where
    updateCh ch px (PixelGray e) = chOp (\ !ch' !e' -> if ch' == ch then e else e') px
    {-# INLINE updateCh #-}
    fromGrays img []     _      = img
    fromGrays img _      []     = img
    fromGrays img (i:is) (c:cs) = fromGrays (zipWith (updateCh c) img i) is cs
    {-# INLINE fromGrays #-}
{-# INLINE fromGrayImages #-}


instance ColorSpace Gray where
  type PixelElt Gray e = e
  data Pixel Gray e = PixelGray !e deriving (Ord, Eq)

  fromChannel = PixelGray
  {-# INLINE fromChannel #-}

  fromElt = PixelGray
  {-# INLINE fromElt #-}

  toElt (PixelGray g) = g
  {-# INLINE toElt #-}

  getPxCh (PixelGray g) _ = g
  {-# INLINE getPxCh #-}
  
  chOp !f (PixelGray g) = PixelGray (f Gray g)
  {-# INLINE chOp #-}
  
  pxOp !f (PixelGray g) = PixelGray (f g)
  {-# INLINE pxOp #-}

  chApp (PixelGray f) (PixelGray g) = PixelGray (f g)
  {-# INLINE chApp #-}

  pxFoldMap f (PixelGray g) = f g `mappend` mempty
  {-# INLINE pxFoldMap #-}

  
instance Show e => Show (Pixel Gray e) where
  show (PixelGray g) = "<Gray:("++show g++")>"



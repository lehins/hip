{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
module Graphics.Image.ColorSpace.Gray (
  Gray(..), Pixel(..)
  ) where

import Prelude hiding (map)
import Graphics.Image.Interface

-- ^ This is a signgle channel colorspace, that is designed to hold any channel
-- from any other colorspace, hence it is not convertible to and from, but
-- rather is here to allow separation of channels from other multichannel
-- colorspaces. If you are looking for a true grayscale colorspace
-- 'Graphics.Image.ColorSpace.Luma.Y' should be used instead.
data Gray = Gray deriving (Eq, Enum)

instance ColorSpace Gray where
  type PixelElt Gray e = e
  data Pixel Gray e = PixelGray !e 

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

  chOp2 !f (PixelGray g1) (PixelGray g2) = PixelGray (f Gray g1 g2)
  {-# INLINE chOp2 #-}
  
  pxOp !f (PixelGray g) = PixelGray (f g)
  {-# INLINE pxOp #-}

  pxOp2 !f (PixelGray g1) (PixelGray g2) = PixelGray (f g1 g2)
  {-# INLINE pxOp2 #-}
instance Show Gray where
  show _ = "Gray"

instance Show e => Show (Pixel Gray e) where
  show (PixelGray g) = "<Gray:("++show g++")>"



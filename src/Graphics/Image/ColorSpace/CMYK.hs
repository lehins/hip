{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             TypeFamilies #-}
-- |
-- Module      : Graphics.Image.ColorSpace.CMYK
-- Copyright   : (c) Alexey Kuleshevich 2016
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
import Graphics.Image.Interface
import Data.Typeable (Typeable)
import qualified Data.Monoid as M (mappend)
import qualified Data.Colour as C
import qualified Data.Colour.Names as C

-- | Cyan, Magenta, Yellow and Black color space.
data CMYK = CyanCMYK -- ^ Cyan
          | MagCMYK  -- ^ Magenta
          | YelCMYK  -- ^ Yellow
          | KeyCMYK  -- ^ Key (Black)
          deriving (Eq, Enum, Typeable)

-- | Cyan, Magenta, Yellow and Black color space with Alpha channel.
data CMYKA = CyanCMYKA  -- ^ Cyan
           | MagCMYKA   -- ^ Magenta
           | YelCMYKA   -- ^ Yellow
           | KeyCMYKA   -- ^ Key (Black)
           | AlphaCMYKA -- ^ Alpha 
           deriving (Eq, Enum, Typeable)


-- | Conversion to `CMYK` color space.
class ColorSpace cs => ToCMYK cs where

  -- | Convert to a `CMYK` pixel.
  toPixelCMYK :: Pixel cs Double -> Pixel CMYK Double

  -- | Convert to a `CMYK` image.
  toImageCMYK :: (Array arr cs Double, Array arr CMYK Double) =>
                 Image arr cs Double
              -> Image arr CMYK Double
  toImageCMYK = map toPixelCMYK
  {-# INLINE toImageCMYK #-}


-- | Conversion to `CMYKA` from another color space with Alpha channel.
class (ToCMYK (Opaque cs), Alpha cs) => ToCMYKA cs where

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

  
instance ColorSpace CMYK where
  type PixelElt CMYK e = (e, e, e, e)
  data Pixel CMYK e = PixelCMYK !e !e !e !e deriving Eq

  fromChannel !e = PixelCMYK e e e e
  {-# INLINE fromChannel #-}

  fromElt !(c, m, y, k) = PixelCMYK c m y k
  {-# INLINE fromElt #-}

  toElt (PixelCMYK c m y k) = (c, m, y, k)
  {-# INLINE toElt #-}

  getPxCh (PixelCMYK c _ _ _) CyanCMYK = c
  getPxCh (PixelCMYK _ m _ _) MagCMYK  = m
  getPxCh (PixelCMYK _ _ y _) YelCMYK  = y
  getPxCh (PixelCMYK _ _ _ k) KeyCMYK  = k
  {-# INLINE getPxCh #-}
  
  chOp !f (PixelCMYK c m y k) =
    PixelCMYK (f CyanCMYK c) (f MagCMYK m) (f YelCMYK y) (f KeyCMYK k)
  {-# INLINE chOp #-}

  pxOp !f (PixelCMYK c m y k) = PixelCMYK (f c) (f m) (f y) (f k)
  {-# INLINE pxOp #-}

  chApp (PixelCMYK fc fm fy fk) (PixelCMYK c m y k) = PixelCMYK (fc c) (fm m) (fy y) (fk k)
  {-# INLINE chApp #-}

  pxFoldMap f (PixelCMYK c m y k) = f c `M.mappend` f m `M.mappend` f y `M.mappend` f k
  {-# INLINE pxFoldMap #-}

  csColour CyanCMYK = C.opaque C.cyan
  csColour MagCMYK  = C.opaque C.magenta
  csColour YelCMYK  = C.opaque C.yellow
  csColour KeyCMYK  = C.opaque C.black


instance ColorSpace CMYKA where
  type PixelElt CMYKA e = (e, e, e, e, e)
  data Pixel CMYKA e = PixelCMYKA !e !e !e !e !e deriving Eq

  fromChannel !e = PixelCMYKA e e e e e
  {-# INLINE fromChannel #-}

  fromElt (c, m, y, k, a) = PixelCMYKA c m y k a
  {-# INLINE fromElt #-}

  toElt (PixelCMYKA c m y k a) = (c, m, y, k, a)
  {-# INLINE toElt #-}

  getPxCh (PixelCMYKA c _ _ _ _) CyanCMYKA  = c
  getPxCh (PixelCMYKA _ m _ _ _) MagCMYKA   = m
  getPxCh (PixelCMYKA _ _ y _ _) YelCMYKA   = y
  getPxCh (PixelCMYKA _ _ _ k _) KeyCMYKA   = k
  getPxCh (PixelCMYKA _ _ _ _ a) AlphaCMYKA = a
  {-# INLINE getPxCh #-}
  
  chOp !f (PixelCMYKA c m y k a) =
    PixelCMYKA (f CyanCMYKA c) (f MagCMYKA m) (f YelCMYKA y) (f KeyCMYKA k) (f AlphaCMYKA a)
  {-# INLINE chOp #-}

  pxOp !f (PixelCMYKA c m y k a) = PixelCMYKA (f c) (f m) (f y) (f k) (f a)
  {-# INLINE pxOp #-}

  chApp (PixelCMYKA fc fm fy fk fa) (PixelCMYKA c m y k a) =
    PixelCMYKA (fc c) (fm m) (fy y) (fk k) (fa a)
  {-# INLINE chApp #-}

  pxFoldMap f (PixelCMYKA c m y k a) =
    f c `M.mappend` f m `M.mappend` f y `M.mappend` f k `M.mappend` f a
  {-# INLINE pxFoldMap #-}

  csColour AlphaCMYKA = C.opaque C.grey
  csColour ch         = csColour $ opaque ch


instance Alpha CMYKA where
  type Opaque CMYKA = CMYK

  getAlpha (PixelCMYKA _ _ _ _ a) = a
  {-# INLINE getAlpha #-}
  
  addAlpha !a (PixelCMYK c m y k) = PixelCMYKA c m y k a
  {-# INLINE addAlpha #-}

  dropAlpha (PixelCMYKA c m y k _) = PixelCMYK c m y k
  {-# INLINE dropAlpha #-}

  opaque CyanCMYKA  = CyanCMYK
  opaque MagCMYKA   = MagCMYK
  opaque YelCMYKA   = YelCMYK
  opaque KeyCMYKA   = KeyCMYK
  opaque AlphaCMYKA = error "Data.Image.ColorSpace.CMYK (Alpha.opaque)"
  

instance Show CMYK where
  show CyanCMYK = "Cyan"
  show MagCMYK  = "Magenta"
  show YelCMYK  = "Yellow"
  show KeyCMYK  = "Black"


instance Show CMYKA where
  show AlphaCMYKA = "Alpha"
  show ch         = show $ opaque ch

  
instance Show e => Show (Pixel CMYK e) where
  show (PixelCMYK c m y k) = "<CMYK:("++show c++"|"++show m++"|"++show y++"|"++show k++")>"


instance Show e => Show (Pixel CMYKA e) where
  show (PixelCMYKA c m y k a) =
    "<CMYKA:("++show c++"|"++show m++"|"++show y++"|"++show k++"|"++show a++")>"




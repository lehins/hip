{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, MultiWayIf #-}
module HIP.Pixel (
  -- * Pixel 
  Pixel(..), pixelGrid,
  -- ** Grayscale
  module HIP.Pixel.Gray, toGray,
  -- ** Color
  -- *** RGB
  module HIP.Pixel.RGB, toRGB,
  -- *** HSI
  module HIP.Pixel.HSI, toHSI,
  -- ** Binary
  module HIP.Binary.Pixel, toBinary,
  -- ** Alpha
  module HIP.Pixel.Alpha,
  -- ** Complex
  module HIP.Complex.Pixel
  ) where

import Prelude hiding (map)
import Data.Int
import Data.Word
import GHC.Float

import HIP.Pixel.Base (Pixel(..))
import HIP.Pixel.Gray
import HIP.Pixel.RGB
import HIP.Pixel.HSI
import HIP.Pixel.Alpha
import HIP.Binary.Pixel
import HIP.Complex.Pixel
import HIP.Interface (Convertible(..), AImage(traverse))


instance AlphaPixel Int where

  
instance AlphaPixel Int8 where

  
instance AlphaPixel Int16 where

  
instance AlphaPixel Int32 where


instance AlphaPixel Int64 where


instance AlphaPixel Word where


instance AlphaPixel Word8 where
  
  
instance AlphaPixel Word16 where

  
instance AlphaPixel Word32 where


instance AlphaPixel Word64 where


instance AlphaPixel Float where


instance AlphaPixel Double where


instance AlphaPixel Gray where  
  

instance AlphaPixel RGB where


instance AlphaPixel HSI where

  
instance ComplexPixel Float where
  apply2c (f1:_) !v1 !v2 = uncurry (:+:) $ f1 v1 v2
  apply2c _ _ px         = error ("Length of the function list should be at least: "
                                  ++ show (arity px))
  {-# INLINE apply2c #-}

  
instance ComplexPixel Double where
  apply2c (f1:_) !v1 !v2 = uncurry (:+:) $ f1 v1 v2
  apply2c _      _   px  = error ("Length of the function list should be at least: "
                                  ++ show (arity px))
  {-# INLINE apply2c #-}
  
  
instance ComplexPixel Gray where
  apply2c (f1:_) (Gray y1) (Gray y2) =
    Gray y1' :+: Gray y2' where (y1', y2') = f1 y1 y2
  apply2c _ _ px = error ("Length of the function list should be at least: "++show (arity px))
  {-# INLINE apply2c #-}

  
instance ComplexPixel RGB where
  apply2c (f1:f2:f3:_) (RGB r1 g1 b1) (RGB r2 g2 b2) =
    RGB r1' g1' b1' :+: RGB r2' g2' b2' where
      !(r1', r2') = f1 r1 r2
      !(g1', g2') = f2 g1 g2
      !(b1', b2') = f3 b1 b2
  apply2c _ _ px = error ("Length of the function list should be at least: "++show (arity px))
  {-# INLINE apply2c #-}

  

instance ComplexPixel HSI where
  apply2c (f1:f2:f3:_) (HSI h1 s1 i1) (HSI h2 s2 i2) =
    HSI h1' s1' i1' :+: HSI h2' s2' i2' where
      (h1', h2') = f1 h1 h2
      (s1', s2') = f2 s1 s2
      (i1', i2') = f3 i1 i2
  apply2c _ _ px = error ("Length of the function list should be at least: "++show (arity px))
  {-# INLINE apply2c #-}


instance (AlphaPixel px, ComplexPixel px) => ComplexPixel (Alpha px) where
  apply2c (f0:rest) (Alpha a1 px1) (Alpha a2 px2) =
    Alpha a1' px1' :+: Alpha a2' px2' where
      (a1', a2') = f0 a1 a2
      px1' :+: px2' = apply2c rest px1 px2
  apply2c _ _ px = error ("Length of the function list should be at least: "++show (arity px))
  {-# INLINE apply2c #-}

------------------------------------
-- Conversion ----------------------
------------------------------------

-- to Gray pixel:

instance Convertible Int Gray where
  convert = Gray . fromIntegral
  {-# INLINE convert #-}


instance Convertible Int8 Gray where
  convert = Gray . fromIntegral
  {-# INLINE convert #-}


instance Convertible Int16 Gray where
  convert = Gray . fromIntegral
  {-# INLINE convert #-}


instance Convertible Int32 Gray where
  convert = Gray . fromIntegral
  {-# INLINE convert #-}


instance Convertible Int64 Gray where
  convert = Gray . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word Gray where
  convert = Gray . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word8 Gray where
  convert = Gray . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word16 Gray where
  convert = Gray . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word32 Gray where
  convert = Gray . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word64 Gray where
  convert = Gray . fromIntegral
  {-# INLINE convert #-}


instance Convertible Float Gray where
  convert = Gray . float2Double
  {-# INLINE convert #-}


instance Convertible Double Gray where
  convert = Gray
  {-# INLINE convert #-}


instance Convertible Gray Gray where
  convert = id
  {-# INLINE convert #-}


instance Convertible RGB Gray where
  convert = rgbToGray
  {-# INLINE convert #-}

  
instance Convertible HSI Gray where
  convert = hsiToGray
  {-# INLINE convert #-}


-- to RGB pixel:


instance Convertible Int RGB where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Int8 RGB where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Int16 RGB where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Int32 RGB where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Int64 RGB where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word RGB where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word8 RGB where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word16 RGB where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word32 RGB where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word64 RGB where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Float RGB where
  convert = fromDouble . float2Double
  {-# INLINE convert #-}


instance Convertible Double RGB where
  convert = fromDouble
  {-# INLINE convert #-}


instance Convertible Gray RGB where
  convert (Gray y) = fromDouble y
  {-# INLINE convert #-}


instance Convertible RGB RGB where
  convert = id
  {-# INLINE convert #-}

  
instance Convertible HSI RGB where
  convert = hsiToRGB
  {-# INLINE convert #-}


-- to HSI pixel:


instance Convertible Int HSI where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Int8 HSI where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Int16 HSI where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Int32 HSI where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Int64 HSI where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word HSI where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word8 HSI where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word16 HSI where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word32 HSI where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Word64 HSI where
  convert = fromDouble . fromIntegral
  {-# INLINE convert #-}


instance Convertible Float HSI where
  convert = fromDouble . float2Double
  {-# INLINE convert #-}


instance Convertible Double HSI where
  convert = fromDouble
  {-# INLINE convert #-}


instance Convertible Gray HSI where
  convert (Gray y) = fromDouble y
  {-# INLINE convert #-}

  
instance Convertible RGB HSI where
  convert = rgbToHSI
  {-# INLINE convert #-}


instance Convertible HSI HSI where
  convert = id
  {-# INLINE convert #-}


-- to Binary pixel


instance Pixel px => Convertible Binary px where
  convert !b = fromDouble $ if isOn b then 0 else 1
  {-# INLINE convert #-}


instance Pixel px => Convertible px Binary where
  convert !b = if b == 0 then on else off
  {-# INLINE convert #-}
  

instance ComplexPixel px => Convertible px (Complex px) where
  convert !px = px :+: fromDouble 0
  {-# INLINE convert #-}


instance ComplexPixel px => Convertible (Complex px) px where
  convert (px :+: _) = px
  {-# INLINE convert #-}


-- | Convert any type of pixel to Gray pixel.
toGray :: Convertible px Gray => px -> Gray
toGray = convert
{-# INLINE toGray #-}


-- | Convert any type of pixel to RGB pixel.
toRGB :: Convertible px RGB => px -> RGB
toRGB = convert
{-# INLINE toRGB #-}


-- | Convert any type of pixel to HSI pixel.
toHSI :: Convertible px HSI => px -> HSI
toHSI = convert
{-# INLINE toHSI #-}


-- | Convert any type of pixel to Binary pixel. If all values within a source
-- pixel equal to @0@ then this function will produce 'on', and 'off'
-- otherwise. This conversion is chosen in order to follow the convention of
-- displaying 'on' Binary pixels as black and 'off' as white. Reverse conversion
-- using 'toGray', 'toRGB' and 'toHSI' follow the same convention.
toBinary :: Convertible px Binary => px -> Binary
toBinary = convert
{-# INLINE toBinary #-}

  
                     
-- | Convert an 'RGB' pixel to 'HSI' pixel.
rgbToHSI :: RGB -> HSI
rgbToHSI (RGB r g b) = HSI h s i where
  !h' = atan2 y x
  !h = if h' < 0 then h' + 2*pi else h'
  !s = if i == 0 then 0 else 1 - minimum [r, g, b] / i
  !i = (r + g + b) / 3
  !x = (2*r - g - b) / 2.449489742783178
  !y = (g - b) / 1.4142135623730951
{-# INLINE rgbToHSI #-}


-- | Convert an 'RGB' pixel to 'Gray' pixel.
rgbToGray :: RGB -> Gray
rgbToGray (RGB r g b) = Gray ((r + g + b)/3)
{-# INLINE rgbToGray #-}


-- | Convert an 'HSI' pixel to 'RGB' pixel.
hsiToRGB :: HSI -> RGB
hsiToRGB (HSI h s i) =
  let !is = i*s
      !second = i - is
      getFirst !a !b = i + is*cos a/cos b
      {-# INLINE getFirst #-}
      getThird !v1 !v2 = i + 2*is + v1 - v2
      {-# INLINE getThird #-}
  in if | h < 2*pi/3 -> let !r = getFirst h (pi/3 - h)
                            !b = second
                            !g = getThird b r
                        in RGB r g b
        | h < 4*pi/3 -> let !g = getFirst (h - 2*pi/3) (h + pi)
                            !r = second
                            !b = getThird r g
                        in RGB r g b
        | h < 2*pi   -> let !b = getFirst (h - 4*pi/3) (2*pi - pi/3 - h)
                            !g = second
                            !r = getThird g b
                        in RGB r g b
        | otherwise  -> error "HSI pixel is not properly normalized" 
{-# INLINE hsiToRGB #-}


-- | Convert an 'HSI' pixel to 'Gray' pixel.
hsiToGray :: HSI -> Gray
hsiToGray (HSI _ _ i) = Gray i
{-# INLINE hsiToGray #-}


-- | This function scales an image by a positive multiplier and draws a grid
-- around the original pixels. It is here simply as useful inspection tool.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage [] "images/frog_eye_grid.png" $ pixelGrid 10 $ crop 51 112 20 20 frog
--
-- <<images/frog.jpg>> <<images/frog_eye_grid.png>>
--
pixelGrid :: AImage img px =>
             Word   -- ^ Positive multiplier.
          -> img px -- ^ Source image.
          -> img px
pixelGrid !(succ . fromIntegral-> k) !img = traverse img getNewDims getNewPx where
  getNewDims !m !n = (1 + m*k, 1 + n*k)
  getNewPx !getPx !i !j = if i `mod` k == 0 || j `mod` k == 0
                          then fromDouble 0.5
                          else getPx ((i - 1) `div` k) ((j - 1) `div` k)
{-# INLINE pixelGrid #-}


--rgbs :: [RGB]
--rgbs = [RGB 1 1 1, RGB 0.5 0.5 0.5, RGB 0 0 0, RGB 1 0 0, RGB 0.75 0.75 0, RGB 0 0.5 0, RGB 0.5 1 1, RGB 0.5 0.5 1, RGB 0.75 0.25 0.75, RGB 0.628 0.643 0.142, RGB 0.255 0.104 0.918, RGB 0.116 0.675 0.255, RGB 0.941 0.785 0.053, RGB 0.704 0.187 0.897, RGB 0.931 0.463 0.316, RGB 0.998 0.974 0.532, RGB 0.099 0.795 0.591, RGB 0.211 0.149 0.597, RGB 0.495 0.493 0.721]

--hsis :: [HSI]
--hsis = fmap (\(HSI h s i) -> HSI (h*pi/180) s i) [HSI 0 0 1, HSI 0 0 0.5, HSI 0 0 0, HSI 0 1 0.333, HSI 60 1 0.5, HSI 120 1 0.167, HSI 180 0.4 0.833, HSI 240 0.25 0.667, HSI 300 0.571 0.583, HSI 61.5 0.699 0.471, HSI 250 0.756 0.426, HSI 133.8 0.667 0.349, HSI 50.5 0.911 0.593, HSI 284.8 0.686 0.596, HSI 13.2 0.446 0.57, HSI 57.4 0.363 0.835, HSI 163.4 0.8 0.495, HSI 247.3 0.533 0.319, HSI 240.4 0.135 0.57]


{-

-- Old and possibly incorrect implementation.

-- | Convert an 'RGB' pixel to 'HSI' pixel.
rgbToHSI' :: RGB -> HSI
rgbToHSI' !(RGB r g b) = HSI h s i where
  !h = if (v1 /= 0.0) then atan2 v2 v1 else 0
  !s = sqrt((v1 * v1) + (v2 * v2))
  !i = (r + g + b)/3
  !v1 = (2.0*r - g - b) / c
  !v2 = (g - b) / c
  !c = 2.44948974278318
{-# INLINE rgbToHSI' #-}

-- | Convert an 'HSI' pixel to 'RGB' pixel.
hsiToRGB :: HSI -> RGB
hsiToRGB !(HSI h s i) = RGB r g b where
  !r  = i + v1
  !g  = i - (v1/2) + v2
  !b  = i - (v1/2) - v2
  !v1 = c * s * (cos h)/3
  !v2 = c * s * (sin h)/2
  !c  = 2.44948974278318
{-# INLINE hsiToRGB #-}

-}

module Graphics.Image.Repa.Fusion (
  -- * Initialization
  make,
  -- * Pixelwise Operations
  -- ** Mapping
  map, imap,
  -- ** Traversing
  traverse, traverse2, traverse3,
  -- ** Zipping
  zipWith,
  -- ** Permutations
  backpermute, transpose,
  -- * Processing
  -- ** Extracting
  crop, scale,
  -- ** Convolution
  -- * Complex
  realPart, imagPart, magnitude, conjugate, makeFilter,
  -- * Conversion
  fromLists, fromArray,
  -- * IO
  readGrayImage, readColorImage
  ) where

import Prelude hiding (map, zipWith)
import qualified Graphics.Image.Interface.Complex as C
import Graphics.Image.Interface.IO
import Graphics.Image.Interface.Processing
import Graphics.Image.Repa.Internal
import Graphics.Image.Repa.Pixel (Pixel, Gray(..), RGB(..), Complex(..), ComplexInner)

-- COMPLEX

{- | Gets real part of a complex image. Full documentation: C.realPart -}
realPart :: (ComplexInner px, Pixel px) =>
            Image (Complex px)
         -> Image px
realPart = C.realPart
{-# INLINE realPart #-}

imagPart :: (ComplexInner px, Pixel px) =>
             Image (Complex px)
          -> Image px
imagPart = C.imagPart
{-# INLINE imagPart #-}


magnitude :: (ComplexInner px, Pixel px) =>
             Image (Complex px)
          -> Image px
magnitude = C.magnitude
{-# INLINE magnitude #-}

{-
complex :: (ComplexInner px, Pixel px) =>
             Image px -- ^ Image representing real part.
          -> Image px -- ^ Image representing imaginary part.
          -> Image (Complex px)
complex = C.complex
{-# INLINE complex #-}
-}

conjugate :: (ComplexInner px, Pixel px) =>
             Image (Complex px)
          -> Image (Complex px)
conjugate = C.conjugate
{-# INLINE conjugate #-}


makeFilter :: (ComplexInner px, Pixel px) =>
              Int
           -> Int
           -> (Int -> Int -> px)
           -> Image px
makeFilter = C.makeFilter
{-# INLINE makeFilter #-}


readGrayImage :: FilePath -> IO (Image Gray)
readGrayImage = readImage
{-# INLINE readGrayImage #-}


readColorImage :: FilePath -> IO (Image RGB)
readColorImage = readImage
{-# INLINE readColorImage #-}

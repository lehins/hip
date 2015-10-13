module Graphics.Image.Repa (
  Image,
  -- * Accessors
  -- ** Image dimensions
  dims, rows, cols,
  -- ** Pixel referencing
  ref, refUnsafe, refDefault, refMaybe,
  -- ** Extracting
  crop,
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
  -- ** Interpolation
  Interpolation(..), interpolate,
  -- ** Geometric
  module Graphics.Image.Processing.Geometric,
  -- ** Convolution
  module Graphics.Image.Processing.Convolution,
  -- * IO
  readGrayImage, readColorImage, setDisplayProgram,
  -- * Conversion
  fromLists, fromArray
  ) where

import Prelude hiding (map, zipWith)
import Data.Array.Repa.Eval (Elt)
import Data.Vector.Unboxed (Unbox)
import Graphics.Image.IO
import Graphics.Image.Interface (Pixel, interpolate, imap)
import Graphics.Image.Interpolation
import Graphics.Image.Pixel (Gray, RGB)
import Graphics.Image.Processing.Geometric
import Graphics.Image.Processing.Convolution
import Graphics.Image.Repa.Internal

-- | Get a pixel at i-th row and j-th column.
ref :: (Elt px, Unbox px, Pixel px) =>
       Image px
    -> Int -> Int
    -> px
ref = index
{-# INLINE ref #-}


-- | Get a pixel at i-th row and j-th column.
refUnsafe :: (Elt px, Unbox px, Pixel px) =>
             Image px
          -> Int -> Int
          -> px
refUnsafe = unsafeIndex
{-# INLINE refUnsafe #-}


-- | Get a pixel at @i@ @j@ location with a default pixel. If @i@ @j@ index is out of
-- bounds, default pixel will be used
refDefault :: (Elt px, Unbox px, Pixel px) =>
              px         -- ^ default pixel that will be returned if out of bounds
           -> Image px   -- ^ image being refrenced
           -> Int -> Int -- ^ @i@ and @j@ index
           -> px
refDefault pxDef img@(dims -> (m, n)) i j =
  if i >= 0 && j >= 0 && i < m && j < n then unsafeIndex img i j else pxDef
{-# INLINE refDefault #-}

                                                 
-- | Get Maybe pixel at @i@ @j@ location. If @i@ @j@ index is out of bounds will return
-- @Nothing@, otherwise @Just px@
refMaybe :: (Elt px, Unbox px, Pixel px) =>
            Image px    -- ^ image being refrenced
         -> Int -> Int  -- ^ @i@ and @j@ index
         -> Maybe (px)
refMaybe img@(dims -> (m, n)) i j =
  if i >= 0 && j >= 0 && i < m && j < n then Just $ unsafeIndex img i j else Nothing
{-# INLINE refMaybe #-}


readGrayImage :: FilePath -> IO (Image Gray)
readGrayImage = readImage
{-# INLINE readGrayImage #-}


readColorImage :: FilePath -> IO (Image RGB)
readColorImage = readImage
{-# INLINE readColorImage #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphics.Image.Sequential (
  Concrete(..), writeImage
  ) where

import Prelude hiding (minmum, maximum)
import Graphics.Image.Definition
import Graphics.Image.Conversion
import Graphics.Image.IO (shouldNormalize, writeArrayImage)
import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.Eval as R
import qualified Data.Vector.Unboxed as V

      
instance Pixel px => Concrete Image px where
  ref (ComputedImage arr) r c = index arr (Z :. r :. c)
  ref (PureImage arr)     _ _ = unsafeIndex arr (Z :. 0 :. 0)
  ref (DelayedImage _)    _ _ =
    error "Only concrete images can have there content referenced."
  {-# INLINE ref #-}
  
  fold = imgFold
  {-# INLINE fold #-}

  compute = imgCompute
  {-# INLINE compute #-}

  toArray (PureImage arr)       = arr
  toArray (ComputedImage arr)   = arr
  toArray img@(DelayedImage _)  = toArray $ compute img
  {-# INLINE toArray #-}
  
  toVector = imgToVector
  {-# INLINE toVector #-}


imgFold :: (Pixel px, Elt px, V.Unbox px) => (px -> px -> px) -> px -> Image px -> px
imgFold op px (PureImage arr) = foldAllS op px arr
imgFold op px (DelayedImage arr) = foldAllS op px arr
imgFold op px (ComputedImage arr) = foldAllS op px arr
{-# INLINE imgFold #-}


imgCompute :: (Pixel px, Elt px, V.Unbox px) => Image px -> Image px 
imgCompute (PureImage arr) = PureImage arr
imgCompute (DelayedImage arr) = ComputedImage $ computeS arr
imgCompute (ComputedImage arr) = ComputedImage arr
{-# INLINE imgCompute #-}


imgToVector :: (Pixel a, Elt a, V.Unbox a) => Image a -> V.Vector a
imgToVector (PureImage arr) = V.singleton $ unsafeIndex arr (Z :. 0 :. 0)
imgToVector (DelayedImage arr) = toUnboxed $ computeS arr
imgToVector (ComputedImage arr) = toUnboxed arr
{-# INLINE imgToVector #-}

writeImage :: (Saveable px) => String -> Image px -> [SaveOptions px] -> IO ()
writeImage !path !img !options =
  writeArrayImage path (toArray $ imgCompute img') options where
  !img' = if shouldNormalize options then normalize img else img
{-# INLINE writeImage #-}

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, BangPatterns, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphics.Image.Parallel (
  Concrete(..), writeImage
  ) where

import Prelude hiding (minmum, maximum)
import Graphics.Image.Definition
import Graphics.Image.IO (Saveable, SaveOptions(Normalize), writeArrayImage)
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
  
  toVector = imgToVector
  {-# INLINE toVector #-}


imgFold :: (Pixel px, Elt px, V.Unbox px) => (px -> px -> px) -> px -> Image px -> px
imgFold op px (PureImage arr) = head $ foldAllP op px arr
imgFold op px (DelayedImage arr) = head $ foldAllP op px arr
imgFold op px (ComputedImage arr) = head $ foldAllP op px arr
{-# INLINE imgFold #-}

imgCompute :: (Pixel px, Elt px, V.Unbox px) => Image px -> Image px 
imgCompute (PureImage arr) = PureImage arr
imgCompute (DelayedImage arr) = deepSeqArray arr' $ ComputedImage arr' where
  arr' = head $ computeP arr
imgCompute img@(ComputedImage _) = img
{-# INLINE imgCompute #-}


imgToVector :: (Pixel px, Elt px, V.Unbox px) => Image px -> V.Vector px
imgToVector (PureImage arr) = V.singleton $ unsafeIndex arr (Z :. 0 :. 0)
imgToVector (DelayedImage arr) = toUnboxed $ head $ computeP arr
imgToVector (ComputedImage arr) = toUnboxed arr
{-# INLINE imgToVector #-}


writeImage :: (Saveable px) => String -> Image px -> [SaveOptions px] -> IO ()
writeImage !path !img !options =
  writeArrayImage path (getArray $ imgCompute img') options where
  !img' = if shouldNormalize options then normalize img else img
  getArray (PureImage arr) = arr
  getArray (ComputedImage arr) = arr
  getArray (DelayedImage _) = error "Buggy imgCompute implementation."
  shouldNormalize [] = True
  shouldNormalize ((Normalize v):_) = v
  shouldNormalize (_:opts) = shouldNormalize opts
{-# INLINE writeImage #-}

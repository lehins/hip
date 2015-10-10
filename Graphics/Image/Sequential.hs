{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Graphics.Image.Sequential (
  compute, fold, sum, maximum, minimum, normalize, toVector, toLists, toArray,
  writeImage, display,
  SaveOption(..)
  ) where

import Prelude hiding (maximum, minimum, sum)
import Graphics.Image.Conversion (Saveable, SaveOption(..))
import Graphics.Image.Interface (Pixel)
import qualified Graphics.Image.Internal as I
import qualified Graphics.Image.IO as IO (writeImage, display)
import Data.Array.Repa as R hiding ((++))
import Data.Vector.Unboxed (Vector, Unbox)


compute :: (Elt px, Unbox px, Pixel px) =>
           I.Image px
        -> I.Image px
compute = I.compute I.Sequential
{-# INLINE compute #-}


fold :: (Elt px, Unbox px, Pixel px) =>
        (px -> px -> px)
     -> px
     -> I.Image px
     -> px
fold = I.fold I.Sequential
{-# INLINE fold #-}


sum :: (Elt px, Unbox px, Pixel px) => I.Image px -> px
       I.Image px
    -> px
sum = I.sum I.Sequential
{-# INLINE sum #-}


maximum :: (Elt px, Unbox px, Pixel px, Ord px) =>
           I.Image px
        -> px
maximum = I.maximum I.Sequential
{-# INLINE maximum #-}


minimum :: (Elt px, Unbox px, Pixel px, Ord px) =>
           I.Image px
        -> px
minimum = I.minimum I.Sequential
{-# INLINE minimum #-}


normalize :: (Elt px, Unbox px, Pixel px, Ord px, Fractional px) =>
             I.Image px
          -> I.Image px
normalize = I.normalize I.Sequential
{-# INLINE normalize #-}


toVector :: (Elt px, Unbox px, Pixel px) =>
            I.Image px
         -> Vector px
toVector = I.toVector I.Sequential
{-# INLINE toVector #-}


toLists :: (Elt px, Unbox px, Pixel px) =>
           I.Image px
        -> [[px]]
toLists = I.toLists I.Sequential
{-# INLINE toLists #-}


toArray :: (Elt px, Unbox px, Pixel px) =>
           I.Image px
        -> Array U DIM2 px
toArray = I.toArray I.Sequential
{-# INLINE toArray #-}


writeImage :: (Saveable I.Image px, Elt px, Unbox px, Pixel px) =>
              FilePath
           -> I.Image px
           -> [SaveOption I.Image px]
           -> IO ()
writeImage !path !img !options = IO.writeImage I.Sequential path img options
{-# INLINE writeImage #-}


display :: (Saveable I.Image px, Elt px, Unbox px, Pixel px) =>
           I.Image px
        -> IO ()
display = IO.display I.Sequential
{-# INLINE display #-}

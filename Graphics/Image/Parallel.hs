{-# LANGUAGE BangPatterns #-}

module Graphics.Image.Parallel (
  compute, fold, maximum, minimum, normalize, toVector, toLists, toArray, writeImage
  ) where

import Prelude hiding (maximum, minimum)
import Graphics.Image.Conversion (Saveable, SaveOptions)
import Graphics.Image.Definition (Pixel)
import qualified Graphics.Image.Internal as I
import qualified Graphics.Image.IO as IO (writeImage)
import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.Eval as R
import Data.Vector.Unboxed (Vector)


compute :: Pixel px =>
           I.Image px
           -> I.Image px
compute = I.compute I.Parallel
{-# INLINE compute #-}


fold :: Pixel px =>
        (px -> px -> px)
        -> px
        -> I.Image px
        -> px
fold = I.fold I.Parallel
{-# INLINE fold #-}


maximum :: (Pixel px, Ord px) =>
           I.Image px
           -> px
maximum = I.maximum I.Parallel
{-# INLINE maximum #-}


minimum :: (Pixel px, Ord px) =>
           I.Image px
           -> px
minimum = I.minimum I.Parallel
{-# INLINE minimum #-}


normalize :: (Pixel px, Ord px) =>
             I.Image px
             -> I.Image px
normalize = I.normalize I.Parallel
{-# INLINE normalize #-}


toVector :: Pixel px =>
            I.Image px
            -> Vector px
toVector = I.toVector I.Parallel
{-# INLINE toVector #-}


toLists :: Pixel px =>
           I.Image px
           -> [[px]]
toLists = I.toLists I.Parallel
{-# INLINE toLists #-}


toArray :: Pixel px =>
           I.Image px
           -> Array U DIM2 px
toArray = I.toArray I.Parallel
{-# INLINE toArray #-}


writeImage :: (Saveable px) =>
              String
              -> I.Image px
              -> [SaveOptions px]
              -> IO ()
writeImage !path !img !options = IO.writeImage I.Parallel path img options

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-} 

module Graphics.Image.Processing.Deconvolution where

import Graphics.Image as I
import Graphics.Image.Interface as I
import Prelude as P

-- | Supplementary function needed to blur an image and test the algorithm.
randomBlur :: Image VS X Double
randomBlur = randomBlur' / scalar (I.sum randomBlur')
  where randomBlur' = fromLists [[0, 0, 4, 3, 2]
                                ,[0, 1, 3, 4, 3]
                                ,[1, 2, 3, 3, 4]
                                ,[0, 1, 2, 1, 0]
                                ,[0, 0, 1, 0, 0]]

-- | 'deconvolve' is used to correct the systematic error of blur (loss of 
-- contrast in smaller features) in images. This particular variant is used
-- when no information about the distortion (blurring and noise) is known.
-- More info about deconvolution at <https://en.wikipedia.org/wiki/Deconvolution>
-- 
-- <<images/frog.jpg>>   <<images/frog_deconvolve.jpg>>
-- 
--  Usage:
-- >>> u0 <- readImage' "images/frog.jpg" :: IO (Image VS RGB Double)
-- >>> let p = randomBlur
-- >>> let d = convolve Edge p u0
-- >>> let us = iterate (deconvolve p d) d  -- Iterative deconvolution
-- >>>     u1 = us !! 1 -- one iteration
-- >>>     u2 = us !! 20 -- twenty iterations
-- >>> let output = makeImage (rows u0, cols u0 * 4)
-- >>>        (\(r,c) ->
-- >>>           let (i, c') = c `quotRem` cols u0
-- >>>           in index ([u0,d,u1,u2] !! i) (r,c'))
-- >>>        :: Image VS RGB Double
-- >>> writeImage "images/frog_deconvolve.jpg" output
-- 
deconvolve :: Image VS X Double
           -> Image VS RGB Double
           -> Image VS RGB Double
           -> Image VS RGB Double
deconvolve p d u
  = u * conv (transpose p) (d / conv p u)
  where conv = convolve Edge


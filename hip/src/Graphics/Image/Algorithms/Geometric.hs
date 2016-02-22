{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Graphics.Image.Algorithms.Geometric (
  flipV, flipH, rotate90, rotate180, rotate270
  ) where

import Graphics.Image.Interface

flipUsing :: Array arr cs e =>
             ((Int, Int) -> (Int, Int) -> (Int, Int)) -> Image arr cs e -> Image arr cs e
flipUsing getNewIndex !img@(dims -> d) = backpermute d (getNewIndex d) img
{-# INLINE flipUsing #-}


-- | Flip an image vertically.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_flipV.jpg" (computeS $ flipV frog) 
--
-- <<images/frog.jpg>> <<images/frog_flipV.jpg>>
--
flipV :: Array arr cs e => Image arr cs e -> Image arr cs e
flipV = flipUsing (\ (m, _) !(i, j) -> (m - 1 - i, j))
{-# INLINE flipV #-}


-- | Flip an image horizontally.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_flipH.jpg" (computeS $ flipH frog) 
--
-- <<images/frog.jpg>> <<images/frog_flipH.jpg>>
--
flipH :: Array arr cs e => Image arr cs e -> Image arr cs e
flipH = flipUsing (\ (_, n) !(i, j) -> (i, n - 1 - j))
{-# INLINE flipH #-}


-- | Rotate an image clockwise by 90°.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_rotate90.jpg" (computeS $ rotate90 frog) 
--
-- <<images/frog.jpg>> <<images/frog_rotate90.jpg>>
--
rotate90 :: Array arr cs e => Image arr cs e -> Image arr cs e
rotate90 = transpose . flipV
{-# INLINE rotate90 #-}


-- | Rotate an image by 180°.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_rotate180.jpg" (computeS $ rotate180 frog) 
--
-- <<images/frog.jpg>> <<images/frog_rotate180.jpg>>
--
rotate180 :: Array arr cs e => Image arr cs e -> Image arr cs e
rotate180 = flipUsing (\ !(m, n) !(i, j) -> (m - 1 - i, n - 1 - j))
{-# INLINE rotate180 #-}


-- | Rotate an image clockwise by 270°.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_rotate270.jpg" (computeS $ rotate270 frog) 
--
-- <<images/frog.jpg>> <<images/frog_rotate270.jpg>>
--
rotate270 :: Array arr cs e => Image arr cs e -> Image arr cs e
rotate270 = transpose . flipH
{-# INLINE rotate270 #-}

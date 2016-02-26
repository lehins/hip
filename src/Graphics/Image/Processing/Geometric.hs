{-# LANGUAGE BangPatterns, ViewPatterns #-}
module Graphics.Image.Processing.Geometric (
  leftToRight, topToBottom,
  crop, flipV, flipH, rotate90, rotate180, rotate270, resize
  ) where

import Graphics.Image.Interface
import Graphics.Image.ColorSpace (Elevator(..))
import Graphics.Image.Processing.Interpolation



-- | Concatenate two images together into one. Both input images must have the
-- same number of rows.
leftToRight :: Array arr cs e => Image arr cs e -> Image arr cs e -> Image arr cs e
leftToRight !img1@(dims -> (_, n1)) !img2 = traverse2 img1 img2 newDims newPx where
  newDims !(m1, _) !(m2, n2)
    | m1 == m2  = (m1, n1 + n2)
    | otherwise = error ("Images must agree in numer of rows, but received: " 
                         ++ show img1 ++ " and " ++ show img2)
  {-# INLINE newDims #-}
  newPx !getPx1 !getPx2 !(i, j) = if j < n1 then getPx1 (i, j) else getPx2 (i, j-n1)
  {-# INLINE newPx #-}
{-# INLINE leftToRight #-}


-- | Concatenate two images together into one. Both input images must have the
-- same number of columns.
topToBottom :: Array arr cs e => Image arr cs e -> Image arr cs e -> Image arr cs e
topToBottom !img1@(dims -> (m1, _)) !img2 = traverse2 img1 img2 newDims newPx where
  newDims !(_, n1) !(m2, n2)
    | n1 == n2  = (m1 + m2, n1)
    | otherwise = error ("Images must agree in numer of columns, but received: "
                         ++ show img1 ++ " and " ++ show img2)
  {-# INLINE newDims #-}
  newPx !getPx1 !getPx2 !(i, j) = if i < m1 then getPx1 (i, j) else getPx2 (i-m1, j)
  {-# INLINE newPx #-}
{-# INLINE topToBottom #-}

-- | Crop an image, i.e. retrieves a sub-image image with @m@ rows and @n@
-- columns. Make sure @(m + i, n + j)@ is not greater than dimensions of a
-- source image.
crop :: Array arr cs e =>
        (Int, Int)     -- ^ @(i, j)@ starting index from within a source image.
     -> (Int, Int)     -- ^ @(m, n)@ dimensions of a new image.
     -> Image arr cs e -- ^ Source image.
     -> Image arr cs e              
crop !(i, j) sz = backpermute sz (\ !(i', j') -> (i' + i, j' + j))
{-# INLINE crop #-}


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




{-
scale :: (Interpolation method, Array arr cs e, Elevator e) =>
         method (Pixel cs e) -- ^ Interpolation method to be used during scaling.
      -> (Double, Double) -- ^ Positive scaling factors.
      -> Image arr cs e -- ^ Source image.
      -> Image arr cs e
--scale _ ((<=0) -> True) _  = error "scale: scaling factor must be greater than 0."
scale !method !(fM, fN) !img = traverse img (const sz') getNewPx where
  k = 1.0
  !(mD, nD) = (fromIntegral m, fromIntegral n)
  !sz@(m, n) = dims img
  !sz' = (round (mD * fM), round (nD * fN))
  !(fM', fN') = ((mD + k) * fM / mD, (nD + k) * fN / nD)
  getNewPx !getPx !(i, j) =
    interpolate method sz getPx 0 ((fromIntegral i + 0.5) / fM - 0.5, (fromIntegral j + 0.5) / fN - 0.5)
    --interpolate method sz getPx 0 (fromIntegral i / fM', fromIntegral j / fN')
  {-# INLINE getNewPx #-}
{-# INLINE scale #-}
-}



resize :: (Interpolation method, Array arr cs e, Elevator e) =>
          method (Pixel cs e) -- ^ Interpolation method to be used during scaling.
       -> (Int, Int)     -- ^ Dimensions of a result image.
       -> Image arr cs e -- ^ Source image.
       -> Image arr cs e -- ^ Reuslt image.
resize !method !sz'@(m', n') !img = traverse img (const sz') getNewPx where
  !sz@(m, n) = dims img
  !(fM, fN) = (fromIntegral m' / fromIntegral m, fromIntegral n' / fromIntegral n)
  getNewPx !getPx !(i, j) =
    interpolate method sz getPx ((fromIntegral i + 0.5) / fM - 0.5, (fromIntegral j + 0.5) / fN - 0.5)
  {-# INLINE getNewPx #-}
{-# INLINE resize #-}

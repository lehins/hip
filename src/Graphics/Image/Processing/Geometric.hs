{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Graphics.Image.Processing.Geometric
-- Copyright   : (c) Alexey Kuleshevich 2016
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Geometric
  ( -- ** Sample
    downsample
  , downsampleRows
  , downsampleCols
  , upsampleRows
  , upsampleCols
  , upsample
  -- ** Append
  , leftToRight
  , topToBottom
  -- ** Canvas
  , translate
  , canvasSize
  , crop
  , superimpose
  -- ** Transform
  -- *** Flip
  , flipV
  , flipH
  -- *** Transpose
  , transpose
  -- *** Rotation
  , rotate90
  , rotate180
  , rotate270
  , rotate
  -- *** Resize
  , resize
  , scale
  ) where

import           Control.Monad.ST
import           Data.Bifunctor
import qualified Data.Massiv.Array        as A
import qualified Data.Massiv.Array.Unsafe as A
import qualified Data.Vector.Unboxed      as VU
import           Graphics.Image.Internal
import           Graphics.Image.Processing.Interpolation
import           Prelude                  hiding (traverse)


-- | Downsample an image. Drop all rows and colums that satisfy the
-- predicates. For example, in order to discard every 5th row and keep every
-- even indexed column:
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> displayImage $ downsample ((0 ==) . (`mod` 5)) odd frog
--
-- <<images/frog.jpg>> <<images/frog_downsampled.jpg>>
--
downsample :: ColorModel cs e =>
              (Int -> Bool) -- ^ Rows predicate
           -> (Int -> Bool) -- ^ Columns predicate
           -> Image cs e -- ^ Source image
           -> Image cs e
downsample mPred nPred = transform getNewDims getNewPx
  where
    getNewDims (Sz2 m n) =
      let rowsIx = VU.filter (not . mPred) $ VU.enumFromN 0 m
          colsIx = VU.filter (not . nPred) $ VU.enumFromN 0 n
          sz = Sz (VU.length rowsIx :. VU.length colsIx)
      in (sz, (rowsIx, colsIx))
    {-# INLINE getNewDims #-}
    getNewPx (rowsIx, colsIx) getPx (i :. j) =
      getPx (VU.unsafeIndex rowsIx i :. VU.unsafeIndex colsIx j)
    {-# INLINE getNewPx #-}
{-# INLINE [~1] downsample #-}


-- | Upsample an image by inserting rows and columns with default pixel into an image. Supplied
-- functions specify how many rows/columns shoud be inserted @(before, after)@ a particular
-- row/column. Returning a negative value in a tuple will result in no upsampling for that
-- row. E.g. insert 2 columns before and 4 columns after every 10th column, while leaving rows count
-- unchanged:
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> displayImage $ upsample (const (0, 0)) (\ k -> if k `mod` 10 == 0 then (2, 4) else (0, 0)) frog
--
-- <<images/frog.jpg>> <<images/frog_upsampled.jpg>>
--
upsample :: ColorModel cs e =>
            Pixel cs e -- ^ Pixel to use for upsampling
         -> (Int -> (Int, Int)) -- ^ How many rows to insert @(above, below)@ a particular row.
         -> (Int -> (Int, Int)) -- ^ How many columns to insert to the @(left, right)@ of a
                                -- particular column.
         -> Image cs e -- ^ Source image
         -> Image cs e
upsample defPx mAdd nAdd = transform getNewDims getNewPx
  where
    getNewDims (m' :. n') = (sz, (sz, rowsIx, colsIx))
      where
        rowsInfoArr = A.makeArrayR A.U Seq m' (bimap (max 0) (max 0) . mAdd)
        colsInfoArr = A.makeArrayR A.U Seq n' (bimap (max 0) (max 0) . nAdd)
        m = A.sum (A.map (uncurry (+)) rowsInfoArr) + m'
        n = A.sum (A.map (uncurry (+)) colsInfoArr) + n'
        sz = m :. n
        rowsIx = makeIndices rowsInfoArr m
        colsIx = makeIndices colsInfoArr n
    {-# INLINE getNewDims #-}
    getNewPx (sz, rowsIx, colsIx) getPx (i :. j) =
      A.handleBorderIndex (Fill defPx) sz getPx (A.unsafeIndex rowsIx i :. A.unsafeIndex colsIx j)
    {-# INLINE getNewPx #-}
    makeIndices a k =
      runST $ do
        marr <- A.unsafeThaw $ A.makeArrayR A.P Seq k (const (-1))
        let writer ci i (l, r) = do
              A.unsafeLinearWrite marr (ci + l) i
              return (ci + l + 1 + r)
        A.ifoldlM_ writer 0 a
        A.unsafeFreeze Seq marr
    {-# INLINE makeIndices #-}
{-# INLINE [~1] upsample #-}


-- | Downsample an image by discarding every odd row.
downsampleRows :: ColorModel cs e => Image cs e -> Image cs e
downsampleRows = downsample odd (const False)
{-# INLINE [~1] downsampleRows #-}


-- | Downsample an image by discarding every odd column.
downsampleCols :: ColorModel cs e => Image cs e -> Image cs e
downsampleCols = downsample (const False) odd
{-# INLINE [~1] downsampleCols #-}


-- | Upsample an image by inserting a row of back pixels after each row of a
-- source image.
upsampleRows :: ColorModel cs e => Image cs e -> Image cs e
upsampleRows = upsample 0 (const (0, 1)) (const (0, 0))
{-# INLINE [~1] upsampleRows #-}


-- | Upsample an image by inserting a column of back pixels after each column of a
-- source image.
upsampleCols :: ColorModel cs e => Image cs e -> Image cs e
upsampleCols = upsample 0 (const (0, 0)) (const (0, 1))
{-# INLINE [~1] upsampleCols #-}



-- | Append two images together into one horisontally. Both input images must have the
-- same number of rows, otherwise error.
leftToRight :: ColorModel cs e => Image cs e -> Image cs e -> Image cs e
leftToRight img1 img2 = computeI (A.append' 1 (delayI img1) (delayI img2))
{-# INLINE [~1] leftToRight #-}


-- | Append two images together into one vertically. Both input images must have the
-- same number of columns, otherwise error.
topToBottom :: ColorModel cs e => Image cs e -> Image cs e -> Image cs e
topToBottom img1 img2 = computeI (A.append' 2 (delayI img1) (delayI img2))
{-# INLINE [~1] topToBottom #-}



-- | Transpose an image
transpose :: ColorModel cs e => Image cs e -> Image cs e
transpose = computeI . A.transpose . delayI
{-# INLINE [~1] transpose #-}


-- | Shift an image towards its bottom right corner by @(delatM :. deltaN)@ rows and
-- columns, while specifying a border resolution strategy.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_translate_wrap.jpg" $ translate Wrap (50 :. 100) frog
-- >>> writeImage "images/frog_translate_edge.jpg" $ translate Edge (50 :. 100) frog
--
-- <<images/frog.jpg>> <<images/frog_translate_wrap.jpg>> <<images/frog_translate_edge.jpg>>
--
translate
  :: ColorModel cs e
  => Border (Pixel cs e) -- ^ Border resolution strategy
  -> (Ix2 -> Ix2) -- ^ Number of rows and columns image will be shifted by.
  -> Image cs e -> Image cs e
translate atBorder fDeltaSz =
  transmute (\sz -> (sz, (sz, fDeltaSz sz))) $ \(sz, (dm :. dn)) getPx (i :. j) ->
    A.handleBorderIndex atBorder sz getPx (i - dm :. j - dn)
{-# INLINE [~1] translate #-}


-- | Change the size of an image. Pixel values and positions will not change, except the ones
-- outside the border, which are handled according to supplied resolution strategy.
--
-- <<images/logo_40.png>>
--
-- For example, it can be used to make a tile from the image above, or simply
-- scale the canvas and place it in a middle:
--
-- >>> logo <- readImageRGBA "images/logo_40.png"
-- >>> writeImage "images/logo_tile.png" $ canvasSize Wrap (\sz -> (sz * (5 :. 7), 0)) logo
-- >>> writeImage "images/logo_center.png" $ canvasSize Edge (\sz -> (sz * (5 :. 7), sz * (2 :. 3))) logo
--
-- <<images/logo_tile.png>> <<images/logo_center.png>>
canvasSize ::
     ColorModel cs e
  => Border (Pixel cs e) -- ^ Border resolution strategy
  -> (Ix2 -> (Ix2, Ix2)) -- ^ Function that returns new dimensions of the image and the offset position
  -> Image cs e -- ^ Source image
  -> Image cs e
canvasSize atBorder fSz =
  transmute
    (\oldSz ->
       let (newSz, offset) = fSz oldSz
       in (newSz, (oldSz, offset))) $ \(sz, dm :. dn) getPx (i :. j) ->
    A.handleBorderIndex atBorder sz getPx (i - dm :. j - dn)
{-# INLINE [~1] canvasSize #-}

-- | Crop an image, i.e. retrieves a sub-image image with @m@ rows and @n@ columns, starting at @i@
-- and @j@ pixel. Make sure @(i + m :. j + n)@ is not greater than dimensions of a source image,
-- otherwise it will result in an error.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_crop.jpg" $ crop (30 :. 80) (const 70) frog
--
-- <<images/frog.jpg>> <<images/frog_crop.jpg>>
--
crop ::
     ColorModel cs e
  => (Ix2 -> (Ix2, Ix2))
  -- ^ Takes dimensions of the source image as an argument and returns @(i `:.` j)@ starting index from
  -- within a source image as well as @(m `:.` n)@ dimensions of a new image.
  -> Image cs e -- ^ Source image.
  -> Image cs e
crop fSz =
  computeI .
  (\arr ->
     let !(ix0, sz) = fSz (A.size arr)
     in A.extract' ix0 sz arr) .
  delayI
{-# INLINE [~1] crop #-}


-- | Place one image on top of a source image, starting at a particular location within
-- a source image.
superimpose ::
     ColorModel cs e
  => Ix2 -- ^ @(i `:.` j)@ starting index from within a source image.
  -> Image cs e -- ^ Image to be positioned on top of the source image.
  -> Image cs e -- ^ Source image.
  -> Image cs e
superimpose ix0 =
  transmute2 (flip (,)) $ \szA getPxA getPxB ix ->
    A.handleBorderIndex (Fill (getPxB ix)) szA getPxA (ix - ix0)
{-# INLINE [~1] superimpose #-}

-- | Flip an image vertically.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_flipV.jpg" $ flipV frog
--
-- <<images/frog.jpg>> <<images/frog_flipV.jpg>>
--
flipV :: ColorModel cs e => Image cs e -> Image cs e
flipV = backpermute dupl (\ (m :. _) (i :. j) -> m - 1 - i :. j)
{-# INLINE [~1] flipV #-}


-- | Flip an image horizontally.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_flipH.jpg" $ flipH frog
--
-- <<images/frog.jpg>> <<images/frog_flipH.jpg>>
--
flipH :: ColorModel cs e => Image cs e -> Image cs e
flipH = backpermute dupl (\ (_ :. n) (i :. j) -> i :. n - 1 - j)
{-# INLINE [~1] flipH #-}


-- | Rotate an image clockwise by 90°.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_rotate90.jpg" $ rotate90 frog
--
-- <<images/frog.jpg>> <<images/frog_rotate90.jpg>>
--
rotate90 :: ColorModel cs e => Image cs e -> Image cs e
rotate90 = transpose . flipV
{-# INLINE [~1] rotate90 #-}


-- | Rotate an image by 180°.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_rotate180.jpg" $ rotate180 frog
--
-- <<images/frog.jpg>> <<images/frog_rotate180.jpg>>
--
rotate180 :: ColorModel cs e => Image cs e -> Image cs e
rotate180 = backpermute dupl (\ sz ix -> sz - ix - 1)
{-# INLINE [~1] rotate180 #-}


-- | Rotate an image clockwise by 270°.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_rotate270.jpg" $ rotate270 frog
--
-- <<images/frog.jpg>> <<images/frog_rotate270.jpg>>
--
rotate270 :: ColorModel cs e => Image cs e -> Image cs e
rotate270 = transpose . flipH
{-# INLINE [~1] rotate270 #-}



-- | Rotate an image clockwise by an angle Θ in radians.
--
-- >>> frog <- readImageRGBA "images/frog.jpg"
-- >>> writeImage "images/frog_rotate330.png" $ rotate Bilinear (Fill 0) (11*pi/6) frog
--
-- <<images/frog.jpg>> <<images/frog_rotate330.png>>
--
rotate :: (ColorModel cs e, Interpolation method) =>
          method -- ^ Interpolation method to be used
       -> Border (Pixel cs e) -- ^ Border handling strategy
       -> Double -- ^ Angle Θ in radians
       -> Image cs e -- ^ Source image
       -> Image cs e -- ^ Rotated image
rotate method border theta' (Image arr) =
  makeImageComp (A.getComp arr) (ceiling mD' :. ceiling nD') $ \(i :. j) ->
    let !(iD, jD) = (fromIntegral i - iDelta + 0.5, fromIntegral j - jDelta + 0.5)
        !i' = iD * cosTheta + jD * sinTheta - 0.5
        !j' = jD * cosTheta - iD * sinTheta - 0.5
    in interpolate method (A.handleBorderIndex border sz (A.index' arr)) (i', j')
  where
    !theta = angle0to2pi (-theta') -- invert angle direction and put it into [0, 2*pi) range
    !sz@(m :. n) = A.size arr
    !mD = fromIntegral m
    !nD = fromIntegral n
    !sinTheta = sin' theta
    !cosTheta = cos' theta
    !sinThetaAbs = abs sinTheta
    !cosThetaAbs = abs cosTheta
    !mD' = mD * cosThetaAbs + nD * sinThetaAbs
    !nD' = nD * cosThetaAbs + mD * sinThetaAbs
    !(iDelta, jDelta) =
      case (sinTheta >= 0, cosTheta >= 0) of
        (True, True) -> (nD * sinTheta, 0) -- I quadrant
        (True, False) -> (mD', -nD * cosTheta) -- II quadrant
        (False, False) -> (-mD * cosTheta, nD') -- III quadrant
        (False, True) -> (0, -mD * sinTheta) -- IV quadrant
{-# INLINE rotate #-}



-- | Resize an image using an interpolation method.
--
-- >>> frog <- readImageRGB "images/frog.jpg"
-- >>> writeImage "images/frog_resize.jpg" $ resize Bilinear Edge (150 :. 480) frog
--
-- <<images/frog.jpg>> <<images/frog_resize.jpg>>
--
resize :: (ColorModel cs e, Interpolation method) =>
          method -- ^ Interpolation method to be used during scaling.
       -> Border (Pixel cs e) -- ^ Border handling strategy
       -> Ix2   -- ^ Dimensions of a result image.
       -> Image cs e -- ^ Source image.
       -> Image cs e -- ^ Result image.
resize method border sz'@(m' :. n') (Image arr) =
  Image (A.makeArray sz' getNewPx)
  where
    sz@(m :. n) = A.size arr
    !fM = fromIntegral m' / fromIntegral m
    !fN = fromIntegral n' / fromIntegral n
    getNewPx (i :. j) =
      interpolate
        method
        (A.handleBorderIndex border sz (A.index' arr))
        ((fromIntegral i + 0.5) / fM - 0.5, (fromIntegral j + 0.5) / fN - 0.5)
    {-# INLINE getNewPx #-}
{-# INLINE resize #-}


-- | Scale an image. Same as resize, except scaling factors are supplied
-- instead of new dimensions.
--
-- @ scale 'Bilinear' 'Edge' (0.5, 2) frog == resize 'Bilinear' 'Edge' (100, 640) frog @
--
scale :: (ColorModel cs e, Interpolation method) =>
         method -- ^ Interpolation method to be used during scaling.
      -> Border (Pixel cs e) -- ^ Border handling strategy
      -> (Double, Double) -- ^ Positive scaling factors.
      -> Image cs e -- ^ Source image.
      -> Image cs e
scale method border (fM, fN) img =
  if fM <= 0 || fN <= 0
    then error "scale: scaling factor must be greater than 0."
    else resize method border (round (fM * fromIntegral m) :. round (fN * fromIntegral n)) img
  where
    (m :. n) = dims img
{-# INLINE scale #-}




----------------------
-- Helper functions --
----------------------

-- | Put an angle into @[0, 2*pi)@ range.
angle0to2pi :: Double -> Double
angle0to2pi !f = f - 2 * pi * floor' (f / (2 * pi))
 where  floor' :: Double -> Double
        floor' !x = fromIntegral (floor x :: Int)
        {-# INLINE floor' #-}
{-# INLINE angle0to2pi #-}


-- | Make sure @sin' pi == 0@ instead of @sin pi == 1.2246467991473532e-16@
sin' :: Double -> Double
sin' a = if abs sinA <= _0 then 0 else sinA
  where !_0 = 10 * sin pi
        !sinA = sin a
{-# INLINE sin' #-}


-- | Make sure @cos' (pi/2) == 0@ instead of @cos (pi/2) == 6.123233995736766e-17@
-- and @cos' (3*pi/2) == 0@ instead of @cos (3*pi/2) == -1.8369701987210297e-16@
cos' :: Double -> Double
cos' a = sin' (a + pi/2)
{-# INLINE cos' #-}


dupl :: t -> (t, t)
dupl sz = (sz, sz)
{-# INLINE dupl #-}

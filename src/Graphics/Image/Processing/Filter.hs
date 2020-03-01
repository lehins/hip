{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- |
-- Module      : Graphics.Image.Processing.Filter
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Image.Processing.Filter
  ( -- * Filter
    Filter (Filter)
  , applyFilter
  , Direction(..)
    -- * Gaussian
  , gaussianLowPass
  , gaussianBlur
    -- * Sobel
  , sobelFilter
  , sobelOperator
    -- * Prewitt
  , prewittFilter
  , prewittOperator
    -- * Laplacian
  , laplacianFilter
    -- * Laplacian of Gaussian
  , logFilter
    -- * Gaussian Smoothing
  , gaussianSmoothingFilter
    -- * Mean
  , meanFilter
    -- * Unsharp Masking
  , unsharpMaskingFilter
  ) where


import           Graphics.Image.Interface              as I
import           Graphics.Image.Processing.Convolution
import           Graphics.Image.ColorSpace               (X)


-- | Filter that can be applied to an image using `applyFilter`.
--
-- @since 1.5.3
data Filter arr cs e = Filter
  { applyFilter :: Image arr cs e -> Image arr cs e -- ^ Apply a filter to an image
  }

-- | Used to specify direction for some filters.
data Direction
  = Vertical
  | Horizontal

-- | Create a Gaussian Filter.
--
-- @since 1.5.3
gaussianLowPass :: (Array arr cs e, Array arr X e, Floating e, Fractional e) =>
                   Int -- ^ Radius
                -> e -- ^ Sigma
                -> Border (Pixel cs e) -- ^ Border resolution technique.
                -> Filter arr cs e
gaussianLowPass !r !sigma border =
  Filter (correlate border gV' . correlate border gV)
  where
    !gV = compute $ (gauss / scalar weight)
    !gV' = compute $ transpose gV
    !gauss = makeImage (1, n) getPx
    !weight = I.fold (+) 0 gauss
    !n = 2 * r + 1
    !sigma2sq = 2 * sigma ^ (2 :: Int)
    getPx (_, j) = promote $ exp (fromIntegral (-((j - r) ^ (2 :: Int))) / sigma2sq)
    {-# INLINE getPx #-}
{-# INLINE gaussianLowPass #-}



-- | Create a Gaussian Blur filter. Radius will be derived from standard
-- deviation: @ceiling (2*sigma)@ and `Edge` border resolution will be
-- utilized. If custom radius and/or border resolution is desired,
-- `gaussianLowPass` can be used instead.
--
-- @since 1.5.3
gaussianBlur :: (Array arr cs e, Array arr X e, Floating e, RealFrac e) =>
                e -- ^ Sigma
             -> Filter arr cs e
gaussianBlur !sigma = gaussianLowPass (ceiling (2*sigma)) sigma Edge
{-# INLINE gaussianBlur #-}


sobelFilter :: (Array arr cs e, Array arr X e) =>
               Direction -> Border (Pixel cs e) -> Filter arr cs e
sobelFilter dir !border =
  Filter (correlate border kernel)
  where
    !kernel =
      case dir of
        Vertical   -> fromLists $ [ [ -1, -2, -1 ]
                                  , [  0,  0,  0 ]
                                  , [  1,  2,  1 ] ]
        Horizontal -> fromLists $ [ [ -1, 0, 1 ]
                                  , [ -2, 0, 2 ]
                                  , [ -1, 0, 1 ] ]
{-# INLINE sobelFilter #-}

-- sobelFilter :: Array arr cs e =>
--                Direction -> Border (Pixel cs e) -> Filter arr cs e
-- sobelFilter dir !border =
--   Filter (convolveCols border cV . convolveRows border rV)
--   where
--     !(rV, cV) =
--       case dir of
--         Vertical   -> ([1, 2, 1], [1, 0, -1])
--         Horizontal -> ([1, 0, -1], [1, 2, 1])
-- {-# INLINE sobelFilter #-}


sobelOperator :: (Array arr cs e, Array arr X e, Floating e) => Image arr cs e -> Image arr cs e
sobelOperator !img = sqrt (sobelX ^ (2 :: Int) + sobelY ^ (2 :: Int))
  where !sobelX = applyFilter (sobelFilter Horizontal Edge) img
        !sobelY = applyFilter (sobelFilter Vertical Edge) img
{-# INLINE sobelOperator #-}


prewittFilter :: (Array arr cs e, Array arr X e) =>
                 Direction -> Border (Pixel cs e) -> Filter arr cs e
prewittFilter dir !border =
  Filter (convolveCols border cV . convolveRows border rV)
  where
    !(rV, cV) =
      case dir of
        Vertical   -> ([1, 1, 1], [1, 0, -1])
        Horizontal -> ([1, 0, -1], [1, 1, 1])
{-# INLINE prewittFilter #-}


prewittOperator :: (Array arr cs e, Array arr X e, Floating e) => Image arr cs e -> Image arr cs e
prewittOperator !img = sqrt (prewittX ^ (2 :: Int) + prewittY ^ (2 :: Int))
  where !prewittX = applyFilter (prewittFilter Horizontal Edge) img
        !prewittY = applyFilter (prewittFilter Vertical Edge) img
{-# INLINE prewittOperator #-}


-- |The Laplacian of an image highlights regions of rapid intensity change
-- and is therefore often used for edge detection. It is often applied to an
-- image that has first been smoothed with something approximating a
-- Gaussian smoothing filter in order to reduce its sensitivity to noise.
-- More info about the algo at <https://homepages.inf.ed.ac.uk/rbf/HIPR2/log.htm>
--
-- <<images/yield.jpg>>   <<images/yield_laplacian.png>>
--
laplacianFilter :: (Array arr cs e, Array arr X e) =>
                   Border (Pixel cs e) -> Filter arr cs e
laplacianFilter !border =
  Filter (correlate border kernel)
  where
    !kernel = fromLists $ [ [ -1, -1, -1 ]   -- Unlike the Sobel edge detector, the Laplacian edge detector uses only one kernel.
                        , [  -1, 8, -1 ]     -- It calculates second order derivatives in a single pass.
                        , [  -1, -1, -1 ]]   -- This is the approximated kernel used for it. (Includes diagonals)
{-# INLINE laplacianFilter #-}

-- | 'Laplacian of Gaussian' (LOG) filter is a two step process of smoothing
-- an image before applying some derivative filter on it. This comes in
-- need for reducing the noise sensitivity while working with noisy
-- datasets or in case of approximating second derivative measurements.
--
-- The LoG operator takes the second derivative of the image. Where the image
-- is basically uniform, the LoG will give zero. Wherever a change occurs, the LoG will
-- give a positive response on the darker side and a negative response on the lighter side.
-- More info about the algo at <https://homepages.inf.ed.ac.uk/rbf/HIPR2/log.htm>
--
-- <<images/yield.jpg>>   <<images/yield_log.png>>
--
logFilter :: (Array arr cs e, Array arr X e) =>
             Border (Pixel cs e) -> Filter arr cs e
logFilter !border =
  Filter (correlate border kernel)
  where
    !kernel = fromLists $ [ [ 0, 1, 1, 2, 2, 2, 1, 1, 0 ]
                          , [  1,  2,  4, 5, 5, 5, 4, 2, 1 ]
                          , [  1,  4,  5, 3, 0, 3, 5, 4, 1 ]
                          , [  2,  5,  3, -12, -24, -12, 3, 5, 2 ]
                          , [  2,  5,  0, -24, -40, -24, 0, 5, 2  ]
                          , [  2,  5,  3, -12, -24, -12, 3, 5, 2  ]
                          , [  1,  4,  5, 3, 0, 3, 5, 4, 1  ]
                          , [  1,  2,  4, 5, 5, 5, 4, 2, 1  ]
                          , [  0,  1,  1, 2, 2, 2, 1, 1, 0  ] ]
{-# INLINE logFilter #-}

-- | The Gaussian smoothing operator is a 2-D convolution operator that is used to
-- `blur' images and remove detail and noise. The idea of Gaussian smoothing is to use
-- this 2-D distribution as a `point-spread' function, and this is achieved by convolution.
-- Since the image is stored as a collection of discrete pixels we need to produce a
-- discrete approximation to the Gaussian function before we can perform the convolution.
-- More info about the algo at <https://homepages.inf.ed.ac.uk/rbf/HIPR2/gsmooth.htm>
--
-- <<images/GSM_gsn_yield_IP.jpg>> <<images/GSM_gsn_yield_OP.png>>
--
gaussianSmoothingFilter :: (Fractional e, Array arr cs e, Array arr X e) =>
                           Border (Pixel cs e) -> Filter arr cs e
gaussianSmoothingFilter !border =
  Filter (I.map (/ 273) . correlate border kernel)
  where
    !kernel = fromLists $ [[ 1, 4, 7, 4, 1 ]     -- Discrete approximation to the Gaussian function.
                          ,[  4, 16, 26, 16, 4 ] -- 273 is the sum of all values in the mask and hence used in rescaling.
                          ,[  7, 26, 41, 26, 7 ]
                          ,[  4, 16, 26, 16, 4 ]
                          ,[ 1, 4, 7, 4, 1 ]]

{-# INLINE gaussianSmoothingFilter #-}


-- | The mean filter is a simple sliding-window spatial filter that replaces the
-- center value in the window with the average (mean) of all the pixel values in
-- the window. The window, or kernel, can be any shape, but this one uses the most
-- common 3x3 square kernel.
-- More info about the algo at <http://homepages.inf.ed.ac.uk/rbf/HIPR2/mean.htm>
--
-- <<images/yield.jpg>>   <<images/yield_mean.png>>
--
meanFilter :: (Fractional e, Array arr cs e, Array arr X e) =>
                           Border (Pixel cs e) -> Filter arr cs e
meanFilter !border =
  Filter (I.map (/ 9) . correlate border kernel)
  where
    !kernel = fromLists $[ [ 1, 1, 1 ]       -- Replace each pixel with the mean value of its neighbors, including itself.
                          , [  1, 1, 1 ]
                          , [  1, 1, 1 ]]

{-# INLINE meanFilter #-}

-- | The unsharp-masking filter is a sharpening operator which derives its name from
-- the fact that it enhances edges (and other high frequency components in an image)
-- via a procedure which subtracts an unsharp, or smoothed, version of an image from
-- the original image. It is commonly used in the photographic and printing industries
-- for crispening edges.
-- More info about the algo at <https://homepages.inf.ed.ac.uk/rbf/HIPR2/unsharp.htm>
--
-- <<images/yield_gray.png>>   <<images/yield_unsharpMasking.png>>
--
unsharpMaskingFilter :: (Fractional e, Array arr cs e, Array arr X e) =>
                           Border (Pixel cs e) -> Filter arr cs e
unsharpMaskingFilter !border =
  Filter (I.map (/256) . correlate border kernel)
  where
    !kernel = fromLists $ [[ -1, -4, -6, -4, -1 ]
                          ,[  -4, -16, -24, -16, -4 ]    -- Uses negative image to create a mask of the original image.
                          ,[  -6, -24, 476, -24, -6 ]    -- The unsharped mask is then combined with the positive (original) image.
                          ,[  -4, -16, -24, -16, -4 ]    -- So, the resulting image is less blurry, i.e clearer.
                          ,[ -1, -4, -6, -4, -1 ]]

{-# INLINE unsharpMaskingFilter #-}

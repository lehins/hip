{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction #-}
module Main where

import Prelude as P
import Graphics.Image as I
import Graphics.Image.Processing

reduce im = downsample . (convolve' . transpose $ kernel) .
            (convolve' kernel) $ im
  where kernel = fromLists [P.map (*(1/20)) [1, 5, 8, 5, 1]]

project im = (convolve' kernel) . (convolve' . transpose $ kernel) . upsample $ im
  where kernel = fromLists [P.map (*(1/10)) [1, 5, 8, 5, 1]]

laplacianPyramid im = lp [] im where
  gaussian = fromLists $ [P.map (*(1/20)) [1, 5, 8, 5, 1]]
  lp ls im' | rows im' == 1 = ls++[im']
            | otherwise = (im' - convolved):lp ls (downsample convolved)
    where convolved = (convolve . transpose $ gaussian) . (convolve gaussian) $ im'

inverseLaplacianPyramid (reverse -> imgs) =
  foldl ilp (head imgs) (tail imgs) where
  gaussian = fromLists $ [P.map (*(1/10)) [1, 5, 8, 5, 1]]
  ilp im1 im2 =
    (convolve gaussian) . (convolve $ transpose gaussian) $ upsample im1 + im2

displayLaplacianPyramid (img:imgs) = foldl appender img imgs where
  r = rows img
  appender i1 i2 = leftToRight i1 $ topToBottom i2 $
                   make (r - rows i2) (cols i2) (\_ _ -> 0)

h0 = fromLists [P.map (/4) [1+sqrt 3, 3+sqrt 3, 3-sqrt 3, 1-sqrt 3]]
h1 = fromLists [P.map (/4) [1-sqrt 3, -3+sqrt 3, 3+sqrt 3, -1-sqrt 3]]
h0t = transpose h0
h1t = transpose h1

data WTransform img = Img img
                    | WImg (WTransform img) img img img

daubechies4 im
  | rows im == 128 = Img im
  | otherwise    = WImg (daubechies4 imClRl) imClRh imChRl imChRh where
  imClRl = downsampleRows . convolve h0t $ imCl
  imClRh = downsampleRows . convolve h1t $ imCl
  imCl   = downsampleCols . convolve h0  $ im
  imChRl = downsampleRows . convolve h0t $ imCh
  imChRh = downsampleRows . convolve h1t $ imCh
  imCh   = downsampleCols . convolve h1  $ im

inverseDaubechies4 (Img img) = img
inverseDaubechies4 (WImg wimg img1 img2 img3) = (imCl + imCh) where
  [h0', h1', h0t', h1t'] = [flipH h0, flipH h1, flipV h0t, flipV h1t]
  imChRh = convolve h1t' . upsampleRows $ img3
  imChRl = convolve h0t' . upsampleRows $ img2
  imClRh = convolve h1t' . upsampleRows $ img1
  imClRl = convolve h0t' . upsampleRows $ inverseDaubechies4 wimg
  imCl = convolve h0' . upsampleCols $ imClRl + imClRh
  imCh = convolve h1' . upsampleCols $ imChRl + imChRh


displayWaveletTransform (Img img) = img
displayWaveletTransform (WImg wimg img1 img2 img3)  =
  topToBottom imTop imBottom where
  imTop = leftToRight (displayWaveletTransform wimg) img1
  imBottom = leftToRight img2 img3

denoiseColorImage im = undefined


main = do
  im <- readGrayImage "tasyah.png"
  --display (project . reduce $ im)
  --display $ displayLaplacianPyramid $ laplacianPyramid im
  --display $ inverseLaplacianPyramid $ laplacianPyramid im
  --display im
  --display $ displayWaveletTransform $ daubechies4 im
  --display im
  display $ inverseDaubechies4 $ daubechies4 im
  return ()
  
  

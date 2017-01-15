{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude as P
import Weigh
import Control.DeepSeq
import Graphics.Image as I
import Graphics.Image.Interface as I
import Data.Array.Repa.Index
import Data.Array.Repa as R
import qualified Data.Vector.Unboxed as V
import Data.Word

data TImage cs e = TImage {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(V.Vector (Pixel cs e))

instance NFData (TImage cs e) where

  rnf (TImage m n v) = m `seq` n `seq` v `deepseq` ()



makeImageT :: (Int, Int) -> (Int -> Pixel RGB Word16) -> TImage RGB Word16
makeImageT (m, n) f =
    TImage m n $ V.generate (m * n) f
{-# INLINE[0] makeImageT #-}

mapT :: (Pixel RGB Word16 -> Pixel RGB Word16) -> TImage RGB Word16 -> TImage RGB Word16
mapT f (TImage m n v) = TImage m n (V.map f v)
{-# INLINE[0] mapT #-}

mapVec :: Weigh ()
mapVec =
  do func "V.map Pixel RGB Word16 1" fVec 1
     func "V.map Pixel RGB Word16 10" fVec 10
     func "V.map Pixel RGB Word16 100" fVec 100
     func "V.map Pixel RGB Word16 1000" fVec 1000
  where fVec n = V.map (+ 3) $ V.map (* 23) ((V.generate n fromIntegral) :: V.Vector (Pixel RGB Word16))


mapImg :: Weigh ()
mapImg =
  do func "I.map Pixel RGB Word16 1" fImg 1
     func "I.map Pixel RGB Word16 10" fImg 10
     func "I.map Pixel RGB Word16 100" fImg 100
     func "I.map Pixel RGB Word16 1000" fImg 1000
  where fImg n = I.map (+ 3) $ I.map (* 23) ((I.makeImage (1, n) (fromIntegral . snd)) :: I.Image VU RGB Word16)

mapImgNum :: Weigh ()
mapImgNum =
  do func "Num Pixel RGB Word16 1" fImg 1
     func "Num Pixel RGB Word16 10" fImg 10
     func "Num Pixel RGB Word16 100" fImg 100
     func "Num Pixel RGB Word16 1000" fImg 1000
  where fImg n = (3 + (23 * ((I.makeImage (1, n) (fromIntegral . snd)) :: I.Image VU RGB Word16)))

mapArr :: Weigh ()
mapArr =
  do func "R.map Pixel RGB Word16 1" fVec 1
     func "R.map Pixel RGB Word16 10" fVec 10
     func "R.map Pixel RGB Word16 100" fVec 100
     func "R.map Pixel RGB Word16 1000" fVec 1000
  where fVec n =
          (R.computeUnboxedS $
           R.map (+ 3) $
           R.map (* 23) $
           (R.fromFunction (Z :. 1 :. n) (fromIntegral . snd') ::
               R.Array R.D R.DIM2 (Pixel RGB Word16))) `R.deepSeqArray` ()
        snd' (Z :. _ :. j) = j


mapImgRSU :: Weigh ()
mapImgRSU =
  do func "I.map RSU Pixel RGB Word16 1" fImg 1
     func "I.map RSU Pixel RGB Word16 10" fImg 10
     func "I.map RSU Pixel RGB Word16 100" fImg 100
     func "I.map RSU Pixel RGB Word16 1000" fImg 1000
  where fImg n = I.compute (I.map (+ 3) $ I.map (* 23) ((I.makeImage (1, n) (fromIntegral . snd')) :: I.Image RSU RGB Word16)) `seq` ()
        snd' !(x, y) = y


mapImgRSUNum :: Weigh ()
mapImgRSUNum =
  do func "Num RSU Pixel RGB Word16 1" fImg 1
     func "Num RSU Pixel RGB Word16 10" fImg 10
     func "Num RSU Pixel RGB Word16 100" fImg 100
     func "Num RSU Pixel RGB Word16 1000" fImg 1000
  where fImg n = I.compute (3 + (23 * (I.makeImage (1, n) (fromIntegral . snd') :: I.Image RSU RGBd Word16))) `seq` ()
        snd' !(x, y) = y


mapImgT :: Weigh ()
mapImgT =
  do func "mapT Pixel RGB Word16 1" fImg 1
     func "mapT Pixel RGB Word16 10" fImg 10
     func "mapT Pixel RGB Word16 100" fImg 100
     func "mapT Pixel RGB Word16 1000" fImg 1000
  where fImg n = mapT (+ 3) $ mapT (* 23) (makeImageT (1, n) fromIntegral)


{-# RULES
"map/map" forall f g img. mapT f (mapT g img) = mapT (f . g) img
 #-}

{-# RULES
"map/make" forall f g sz. mapT f (makeImageT sz g) = makeImageT sz (f . g)
 #-}


main :: IO ()
main = mainWith $ do mapVec
                     mapImg
                     mapImgNum
                     mapArr
                     mapImgRSU
                     mapImgRSUNum
                     mapImgT




-- foldlT :: V.Unbox b => (a -> b -> a) -> a -> TImage b -> a
-- foldlT f a (TImage v) = V.foldl' f a v

-- foldVec :: Weigh ()
-- foldVec =
--   do func "V.foldl Pixel RGB Word16 1" fVec 1
--      func "V.foldl Pixel RGB Word16 10" fVec 10
--      func "V.foldl Pixel RGB Word16 100" fVec 100
--      func "V.foldl Pixel RGB Word16 1000" fVec 1000
--   where fVec n = V.foldl' (+) (PixelRGB 0 0 0) ((V.generate n fromIntegral) :: V.Vector (Pixel RGB Word16))


-- foldImg :: Weigh ()
-- foldImg =
--   do func "I.foldl Pixel RGB Word16 1" fImg 1
--      func "I.foldl Pixel RGB Word16 10" fImg 10
--      func "I.foldl Pixel RGB Word16 100" fImg 100
--      func "I.foldl Pixel RGB Word16 1000" fImg 1000
--   where fImg n = I.foldl (+) (PixelRGB 0 0 0) ((I.makeImage (1, n) (fromIntegral . snd)) :: I.Image VU RGB Word16)


-- foldImgT :: Weigh ()
-- foldImgT =
--   do func "foldlT Pixel RGB Word16 1" fImg 1
--      func "foldlT Pixel RGB Word16 10" fImg 10
--      func "foldlT Pixel RGB Word16 100" fImg 100
--      func "foldlT Pixel RGB Word16 1000" fImg 1000
--   where fImg n = foldlT (+) (PixelRGB 0 0 0) (makeImageT (1, n) fromIntegral)


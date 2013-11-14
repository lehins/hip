{-# LANGUAGE ViewPatterns #-}
module Data.Image where


import Data.Image.Base
import Data.Image.Gray
import Data.Image.Color
import Data.Image.Internal


dim img = (width img, height img)

imageMap op img = liftI op img

--imageZipWith op (Image px1) (Image px2) =
--  liftI2 op img1 img2

  --imageFold op px (ColorImage img) = rFold op px img


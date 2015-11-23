{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, TypeFamilies #-}
module HIP.Pixel.Base where

import HIP.Interface (Pixel(..))

instance Pixel Double where
  type Inner Double = Double

  pixel = id
  {-# INLINE pixel #-}

  pxOp = ($)
  {-# INLINE pxOp #-}

  pxOp2 = ((.).(.)) id
  {-# INLINE pxOp2 #-}

  size _ = 1
  {-# INLINE size #-}

  ref 0 !y = y
  ref n px = error ("Referencing "++show n++"is out of bounds for "++showType px)
  {-# INLINE ref #-}

  strongest = id
  {-# INLINE strongest #-}

  weakest = id
  {-# INLINE weakest #-}

  showType _ = "Double"
  
  

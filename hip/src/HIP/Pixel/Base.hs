{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}
module HIP.Pixel.Base where

import Data.Int
import Data.Word

baseRef :: (Pixel px) => Int -> px -> px
baseRef 0 !y = y
baseRef n px = error ("Referencing "++show n++"is out of bounds for "++showType px)
{-# INLINE baseRef #-}

baseApply :: Pixel px => [(px -> px)] -> px -> px
baseApply !(f:_) !d = f d
baseApply _ px = error ("Length of the function list should be at least: "++(show $ arity px))
{-# INLINE baseApply #-}

baseApply2 :: Pixel px => [(px -> px -> t)] -> px -> px -> t
baseApply2 !(f:_) !d1 !d2 = f d1 d2
baseApply2 _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
{-# INLINE baseApply2 #-}


class (Eq px, Num px, Show px,
       Eq (Inner px), Num (Inner px), Show (Inner px), Ord (Inner px)
      ) => Pixel px where
  -- | Internal type used for pixel values.
  type Inner px :: *
  
  pixel :: Inner px -> px
       
  arity :: px -> Int

  ref :: Int -> px -> Inner px

  apply :: [(Inner px -> Inner px)] -> px -> px

  apply2 :: [(Inner px -> Inner px -> Inner px)] -> px -> px -> px

  showType :: px -> String


instance Pixel Float where
  type Inner Float = Float

  pixel = id
  {-# INLINE pixel #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  showType _ = "Float"
  
  
instance Pixel Double where
  type Inner Double = Double

  pixel = id
  {-# INLINE pixel #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  showType _ = "Double"



instance Pixel Int where
  type Inner Int = Int

  pixel = id
  {-# INLINE pixel #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  showType _ = "Int"
  
  
instance Pixel Int8 where
  type Inner Int8 = Int8

  pixel = id
  {-# INLINE pixel #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  showType _ = "Int8"
  
  
instance Pixel Int16 where
  type Inner Int16 = Int16

  pixel = id
  {-# INLINE pixel #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  showType _ = "Int16"
  
  
instance Pixel Int32 where
  type Inner Int32 = Int32

  pixel = id
  {-# INLINE pixel #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  showType _ = "Int32"


instance Pixel Int64 where
  type Inner Int64 = Int64

  pixel = id
  {-# INLINE pixel #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  showType _ = "Int64"
  
  
  
instance Pixel Word where
  type Inner Word = Word

  pixel = id
  {-# INLINE pixel #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  showType _ = "Word"
  
  
instance Pixel Word8 where
  type Inner Word8 = Word8

  pixel = id
  {-# INLINE pixel #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  showType _ = "Word8"
  
  
instance Pixel Word16 where
  type Inner Word16 = Word16

  pixel = id
  {-# INLINE pixel #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  showType _ = "Word16"
  
  
instance Pixel Word32 where
  type Inner Word32 = Word32

  pixel = id
  {-# INLINE pixel #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  showType _ = "Word32"


instance Pixel Word64 where
  type Inner Word64 = Word64

  pixel = id
  {-# INLINE pixel #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  showType _ = "Word64"
  
  
  
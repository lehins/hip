{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}
module HIP.Pixel.Base (
  Pixel(..)
  ) where

import Data.Int
import Data.Word
import Data.Data
import GHC.Float

baseRef :: (Pixel px) => px -> Int -> px
baseRef !y 0 = y
baseRef px n = error ("Referencing "++show n++"is out of bounds for "++show (typeOf px))
{-# INLINE baseRef #-}

baseUpdate :: (Pixel px, px ~ Channel px) => px -> Int -> Channel px -> px
baseUpdate _  0 c  = c
baseUpdate px n _ = error ("Updating "++show n++"is out of bounds for "++show (typeOf px))
{-# INLINE baseUpdate #-}

baseApply :: Pixel px => [(px -> px)] -> px -> px
baseApply !(f:_) !d = f d
baseApply _ px = error ("Length of the function list should be at least: "++(show $ arity px))
{-# INLINE baseApply #-}

baseApply2 :: Pixel px => [(px -> px -> t)] -> px -> px -> t
baseApply2 !(f:_) !d1 !d2 = f d1 d2
baseApply2 _ _ px = error ("Length of the function list should be at least: "++(show $ arity px))
{-# INLINE baseApply2 #-}


class (Eq px, Num px, Show px, Data px, Typeable px, Ord (Channel px), Data (Channel px)
      ) => Pixel px where
  -- | Internal type used for describing precision of pixel's channel values.
  type Channel px :: *

  {-# MINIMAL fromDouble, arity, ref, update, apply, apply2, maxChannel, minChannel #-}

  -- | Constructs a pixel from a 'Double' using the same value for all channels. ex:
  --
  -- @fromDouble 0.3 :: HSI == HSI 0.3 0.3 0.3@
  --
  fromDouble :: Double -> px

  arity :: px -> Int

  ref :: px -> Int -> Channel px

  update :: px -> Int -> Channel px -> px

  apply :: [(Channel px -> Channel px)] -> px -> px

  apply2 :: [(Channel px -> Channel px -> Channel px)] -> px -> px -> px

  -- | Retrieves the maximum channel, ex:
  --
  -- @maxChannel $ RGB 0.1 0.3 0.2 == 0.3@
  maxChannel :: px -> Channel px
  
  -- | Retrieves the minimum channel, ex:
  --
  -- @minChannel $ RGB 0.1 0.3 0.2 == 0.1@
  minChannel :: px -> Channel px

  -- | Constructs a pixel from a single channel replicating the same value for
  -- all channels. ex:
  --
  -- @fromChannel 0.2 :: Complex HSI == HSI 0.2 0.2 0.2 :+: HSI 0.2 0.2 0.2@
  --
  fromChannel :: Channel px -> px
  fromChannel c = fromConstrB (fromConstr $ toConstr c) (toConstr c)
  {-# INLINE fromChannel #-}
    

instance Pixel Float where
  type Channel Float = Float

  fromDouble = double2Float
  {-# INLINE fromDouble #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  update = baseUpdate
  {-# INLINE update #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  maxChannel = id
  {-# INLINE maxChannel #-}

  minChannel = id
  {-# INLINE minChannel #-}

  fromChannel = id
  {-# INLINE fromChannel #-}

instance Pixel Double where
  type Channel Double = Double

  fromDouble = id
  {-# INLINE fromDouble #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  update = baseUpdate
  {-# INLINE update #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  maxChannel = id
  {-# INLINE maxChannel #-}

  minChannel = id
  {-# INLINE minChannel #-}

  fromChannel = id
  {-# INLINE fromChannel #-}


instance Pixel Int where
  type Channel Int = Int

  fromDouble = round
  {-# INLINE fromDouble #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  update = baseUpdate
  {-# INLINE update #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  maxChannel = id
  {-# INLINE maxChannel #-}

  minChannel = id
  {-# INLINE minChannel #-}

  fromChannel = id
  {-# INLINE fromChannel #-}

  
instance Pixel Int8 where
  type Channel Int8 = Int8

  fromDouble = round
  {-# INLINE fromDouble #-}

  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  update = baseUpdate
  {-# INLINE update #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  maxChannel = id
  {-# INLINE maxChannel #-}

  minChannel = id
  {-# INLINE minChannel #-}

  fromChannel = id
  {-# INLINE fromChannel #-}

  
instance Pixel Int16 where
  type Channel Int16 = Int16

  fromDouble = round
  {-# INLINE fromDouble #-}
  
  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  update = baseUpdate
  {-# INLINE update #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  maxChannel = id
  {-# INLINE maxChannel #-}

  minChannel = id
  {-# INLINE minChannel #-}

  fromChannel = id
  {-# INLINE fromChannel #-}

  
instance Pixel Int32 where
  type Channel Int32 = Int32

  fromDouble = round
  {-# INLINE fromDouble #-}
  
  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  update = baseUpdate
  {-# INLINE update #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  maxChannel = id
  {-# INLINE maxChannel #-}

  minChannel = id
  {-# INLINE minChannel #-}

  fromChannel = id
  {-# INLINE fromChannel #-}

  
instance Pixel Int64 where
  type Channel Int64 = Int64

  fromDouble = round
  {-# INLINE fromDouble #-}
  
  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  update = baseUpdate
  {-# INLINE update #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  maxChannel = id
  {-# INLINE maxChannel #-}

  minChannel = id
  {-# INLINE minChannel #-}

  fromChannel = id
  {-# INLINE fromChannel #-}

  
instance Pixel Word where
  type Channel Word = Word

  fromDouble = round
  {-# INLINE fromDouble #-}
  
  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  update = baseUpdate
  {-# INLINE update #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  maxChannel = id
  {-# INLINE maxChannel #-}

  minChannel = id
  {-# INLINE minChannel #-}
  
  fromChannel = id
  {-# INLINE fromChannel #-}


instance Pixel Word8 where
  type Channel Word8 = Word8

  fromDouble = round
  {-# INLINE fromDouble #-}
  
  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  update = baseUpdate
  {-# INLINE update #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  maxChannel = id
  {-# INLINE maxChannel #-}

  minChannel = id
  {-# INLINE minChannel #-}
  
  fromChannel = id
  {-# INLINE fromChannel #-}

  
instance Pixel Word16 where
  type Channel Word16 = Word16

  fromDouble = round
  {-# INLINE fromDouble #-}
  
  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  update = baseUpdate
  {-# INLINE update #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  maxChannel = id
  {-# INLINE maxChannel #-}

  minChannel = id
  {-# INLINE minChannel #-}
  
  fromChannel = id
  {-# INLINE fromChannel #-}


instance Pixel Word32 where
  type Channel Word32 = Word32

  fromDouble = round
  {-# INLINE fromDouble #-}
  
  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  update = baseUpdate
  {-# INLINE update #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  maxChannel = id
  {-# INLINE maxChannel #-}

  minChannel = id
  {-# INLINE minChannel #-}

  fromChannel = id
  {-# INLINE fromChannel #-}


instance Pixel Word64 where
  type Channel Word64 = Word64

  fromDouble = round
  {-# INLINE fromDouble #-}
  
  arity _ = 1
  {-# INLINE arity #-}

  ref = baseRef
  {-# INLINE ref #-}

  update = baseUpdate
  {-# INLINE update #-}

  apply = baseApply
  {-# INLINE apply #-}

  apply2 = baseApply2
  {-# INLINE apply2 #-}

  maxChannel = id
  {-# INLINE maxChannel #-}

  minChannel = id
  {-# INLINE minChannel #-}

  fromChannel = id
  {-# INLINE fromChannel #-}

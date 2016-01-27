{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module Graphics.Image (
  ) where

import qualified Prelude
import Graphics.Image.Interface
import Graphics.Image.Accelerate.Internal
--import Graphics.Image.Repa.Internal


foo :: (IEq arr cs e, If arr cs e, Array arr cs e, Prelude.Num e) => Image arr cs e -> Image arr cs e
foo img = map (\e -> iif (e ==. 2) 4 e) img


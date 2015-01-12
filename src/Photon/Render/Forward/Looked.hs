-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Photon.Render.Forward.Looked where

import Data.Monoid ( Monoid(..) )
import Photon.Render.Camera ( GPUCamera )
import Photon.Render.Forward.Accumulation
import Photon.Render.Forward.Lighting
import Photon.Render.Forward.Lit ( Lit(..) )
import Photon.Render.Forward.Shadowing

newtype Looked = Looked { unLooked :: Lighting -> Shadowing -> Accumulation -> IO () }

instance Monoid Looked where
  mempty = Looked $ \_ _ _ -> return ()
  Looked f `mappend` Looked g = Looked $ \l s a -> f l s a >> g l s a

look :: GPUCamera -> Lit -> Looked
look gpucam lit = Looked look_
  where
    look_ lighting shadowing accumulation = do
      purgeAccumulationFramebuffer accumulation
      pushCameraToLighting lighting gpucam
      unLit lit lighting shadowing accumulation

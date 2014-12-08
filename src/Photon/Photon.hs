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

module Photon.Photon (
    -- *
  ) where

import Control.Lens
import Control.Monad.Trans.State ( StateT )
import Photon.Core.Effect
import Photon.Core.Light ( Light )
import Photon.Render.OpenGL.State
import Photon.Utils.FreeList ( FreeList )

newtype Photon m a = Photon { runPhoton :: StateT (FreeLists,OpenGLSt) m a }

data FreeLists = FreeLists {
    _flLights    :: FreeList
  , _flMaterials :: FreeList
  , _flMeshes    :: FreeList
  }

makeLenses ''FreeLists

class Manageable a where
  manage :: a -> Photon (Managed a)
  drop :: Managed a -> Photon ()

instance Manageable Light where
  manage a = do
    (h,fl) <- uses (_1.flLights) nextFree
    _1 . flLights .~ fl
    return (Managed (H h) a)
  drop (Managed (H h) _) = _1 . flLights %~ recycleFree h

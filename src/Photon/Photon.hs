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

instance Manager Light Photon where
  manage = manage_ (Photon $ uses . _1 . flLights) (\f -> Photon $ uses . _1 . flLights %~ f)

manage_ :: Photon FreeList
        -> ((FreeList -> FreeList) -> Photon ())
        -> a
        -> Photon (Managed a)
manage_ getFreeList' modifyFreeList' a = do
  (h,fl) <- fmap nextFree getFreeList'
  modifyFreeList' (const fl)
  return (Managed (H h) a)

drop_ :: ((FreeList -> FreeList) -> Photon ())
     -> Managed a
     -> Photon ()
drop_ modifyFreeList' (Managed (H h) _) = modifyFreeList' (recycleFree h)

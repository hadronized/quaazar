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

module Photon.Render.OpenGL.Forward.Cache where

import Control.Applicative
import Control.Monad ( forM )
import Control.Lens
import Data.Vector as V ( Vector, foldl, fromList )
import Photon.Core.Entity ( Entity, entityName )
import Photon.Core.Light ( Light )
import Photon.Core.Scene
import Photon.Render.OpenGL.Forward.Camera
import Photon.Render.OpenGL.Forward.Material
import Photon.Render.OpenGL.Forward.Mesh

data Cache = Cache {
    -- |
    _cacheCamera  :: FCamera
    -- |
  , _cacheLights  :: Vector (Light,[Entity IndexPath])
    -- |
  , _cacheObjects :: Vector (FMaterial,Vector (FMesh,[Entity IndexPath]))
  } deriving (Eq,Show)

makeLenses ''Cache

-- |Turns scene relations into an OpenGL /cache/, used to accelerate
-- rendering.
generateCache :: SceneRel a -> IO Cache
generateCache (SceneRel cam ligs objs) =
    Cache <$> pure fcam <*> pure fligs <*> fobjs
  where
    fcam  = forwardCamera cam
    fligs = fromList $ map ((,[]) . snd) ligs
    fobjs =
      fmap fromList . forM objs $ \(mat,mshs) ->
        fmap ((forwardMaterial mat,) . fromList) $ mapM (fmap (,[]) . forwardMesh . snd) mshs

cacheLight :: Cache -> Entity IndexPath -> Cache
cacheLight cch lig = cch & cacheLights . ix li . _2 %~ (:) lig
  where
    [li] = unIndexPath (lig^.entityName)

cacheObject :: Cache -> Entity IndexPath -> Cache
cacheObject cch obj = cch & cacheObjects . ix mati . _2 . ix mshi . _2 %~ (:) obj
  where
    [mati,mshi] = unIndexPath (obj^.entityName)

cache :: Scene IndexPath -> Cache -> Cache
cache ents cch = V.foldl cacheObject (V.foldl cacheLight cch ligs) objs
  where
    ligs = ents^.lights
    objs = ents^.objects

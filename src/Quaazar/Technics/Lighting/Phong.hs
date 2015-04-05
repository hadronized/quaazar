{-# LANGUAGE OverloadedStrings #-}

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

module Quaazar.Technics.Lighting.Phong where

import Control.Applicative hiding ( empty )
import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Aeson
import Data.IORef ( modifyIORef, newIORef, readIORef, writeIORef )
import Data.Map as M ( delete, empty, insert, lookup )
import Numeric.Natural ( Natural )
import Quaazar.Core.Loader ( Load(..) )
import Quaazar.Core.Resource ( Manager(..), Resource(..) )
import Quaazar.Core.Texture ( Texture )
import Quaazar.Render.GLSL
import Quaazar.Render.Shader
import Quaazar.Render.Texture ( GPUTexture )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

data PhongMaterial = PhongMaterial {
    diffuseMap  :: GPUTexture
  , specularMap :: GPUTexture
  , glossMap    :: GPUTexture
  }

data PhongMaterialManifest = PhongMaterialManifest String String String

instance FromJSON PhongMaterialManifest where
  parseJSON = withObject "phong material" $ \o ->
    PhongMaterialManifest
      <$> o .: "diffuse"
      <*> o .: "specular"
      <*> o .: "gloss"

instance Load PhongMaterialManifest where
  loadRoot = const "materials"
  loadExt = const "qmat"

instance Resource (Manager () Texture,Manager (Manager () Texture) GPUTexture) PhongMaterial where
  manager root = do
      ref <- liftIO $ newIORef empty
      return $ Manager (retrieve_ ref) (release_ ref)
    where
      retrieve_ ref (texMgr,gtexMgr) name = do
        mp <- liftIO $ readIORef ref
        case M.lookup name mp of
          Just mat -> return mat
          Nothing -> do
            mat <- do
              PhongMaterialManifest dp sp gp <- load root name
              PhongMaterial
                <$> retrieve gtexMgr texMgr dp
                <*> retrieve gtexMgr texMgr sp
                <*> retrieve gtexMgr texMgr gp
            liftIO . writeIORef ref $ insert name mat mp
            return mat
      release_ ref name = liftIO . modifyIORef ref $ delete name

phong :: (MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
      => m (GPUProgram PhongMaterial)
phong = gpuProgram phongVS Nothing Nothing phongFS $ \mat -> do
    uniform phongDiffMapSem $= (diffuseMap mat,0 :: Natural)
    uniform phongSpecMapSem $= (specularMap mat,1 :: Natural)
    uniform phongGlossMapSem $= (glossMap mat,2 :: Natural)

phongDiffMapSem :: Natural
phongDiffMapSem = 10

phongSpecMapSem :: Natural
phongSpecMapSem = 11

phongGlossMapSem :: Natural
phongGlossMapSem = 12

phongVS :: String
phongVS = unlines
  [
    "#version 430 core"

  , "layout (location = 0) in vec3 co;"
  , "layout (location = 1) in vec3 no;"
  , "layout (location = 2) in vec2 uv;"

  , declUniform camProjViewSem "mat4 projView"
  , declUniform modelSem "mat4 model"

  , "out vec3 vco;"
  , "out vec3 vno;"
  , "out vec2 vuv;"

  , "void main() {"
  , "  vco = (model * vec4(co,1.)).xyz;"
  , "  vno = normalize((transpose(inverse(model)) * vec4(no,1.)).xyz);"
  , "  vuv = uv;"
  , "  gl_Position = projView * vec4(vco,1.);"
  , "}"
  ]

phongFS :: String
phongFS = unlines
  [
    "#version 430 core"

  , "in vec3 vco;"
  , "in vec3 vno;"
  , "in vec2 vuv;"

  , declUniform eyeSem "vec3 eye"
  , declUniform phongDiffMapSem "sampler2D phongDiffMap"
  , declUniform phongSpecMapSem "sampler2D phongSpecMap"
  , declUniform phongGlossMapSem "sampler2D phongGlossMap"
    -- ambient lighting
  , declUniform ligAmbColSem "vec3 ligAmbCol"
  , declUniform ligAmbPowSem "float ligAmbPow"
    -- omni lights
  , "struct Omni {"
  , "  vec3 pos;"
  , "  vec3 col;"
  , "  float pow;"
  , "  float rad;"
  , " };"

  , declUniformBlock ligOmniSSBOBP "OmniBuffer { Omni ligs[]; } omniBuffer"
  , declUniform ligOmniNbSem "uint ligOmniNb"

  , "out vec4 frag;"

  , "void main() {"
  , "  vec3 phongDiff = texture(phongDiffMap, vuv).rgb;"
  , "  vec3 phongSpec = texture(phongSpecMap, vuv).rgb;"
  , "  float phongGloss = texture(phongGlossMap, vuv).r;"
  , "  vec3 v = normalize(eye - vco);"

    -- ambient lighting
  , "  vec3 ambient = ligAmbCol * phongDiff * ligAmbPow;"

    -- omni lights
  , "  vec3 omni = vec3(0.,0.,0.);"
  , "  for (uint i = 0u; i < ligOmniNb; ++i) {"
  , "    vec3 ligCol = omniBuffer.ligs[i].col;"
  , "    float ligPow = omniBuffer.ligs[i].pow;"
  , "    float ligRad = omniBuffer.ligs[i].rad;"
  , "    vec3 ligToVertex = omniBuffer.ligs[i].pos - vco;"
  , "    vec3 ligDir = normalize(ligToVertex);"
  , "    vec3 r = normalize(reflect(-ligDir,vno));"
  , "    vec3 diff = max(0.,dot(vno,ligDir)) * ligCol * phongDiff;"
  , "    vec3 spec = pow(max(0.,dot(r,v)), 1. + phongGloss * 1000.) * ligCol * phongSpec;"
  , "    float atten = ligPow / (pow(1. + length(ligToVertex)/ligRad,2.));"
  , "    omni += atten * (diff + spec);"
  , "  }"

  , "  frag = clamp(vec4(ambient + omni,1.), vec4(0.,0.,0.,0.), vec4(1.,1.,1.,1.));"
  , "}"
  ]

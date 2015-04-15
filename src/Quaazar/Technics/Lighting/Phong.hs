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
import Quaazar.Render.GL.Shader ( Program', Semantics, ($=), buildProgram
                                , uniform )
import Quaazar.Render.GL.Texture ( CompareFunc, Filter, Texture2D
                                 , Texture2DManager, Wrap )
import Quaazar.Render.GLSL
import Quaazar.System.Loader
import Quaazar.System.Resource ( Manager(..), Resource(..) )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

data PhongMaterial = PhongMaterial {
    diffuseMap  :: Texture2D
  , specularMap :: Texture2D
  , glossMap    :: Texture2D
  }

data PhongMaterialManifest = PhongMaterialManifest String String String

instance FromJSON PhongMaterialManifest where
  parseJSON = withObject "phong material" $ \o ->
    PhongMaterialManifest
      <$> o .: "diffuse"
      <*> o .: "specular"
      <*> o .: "gloss"

instance Load () PhongMaterialManifest where
  loadRoot = const "materials"
  loadExt = const "qmat"

instance Resource (Texture2DManager,Wrap,Filter,Maybe CompareFunc,Natural,Natural) PhongMaterial where
  manager root = do
      ref <- liftIO $ newIORef empty
      return $ Manager (retrieve_ ref) (release_ ref)
    where
      retrieve_ ref (texMgr,wrap,flt,cmpf,baseLvl,maxLvl) name = do
        mp <- liftIO $ readIORef ref
        case M.lookup name mp of
          Just mat -> return mat
          Nothing -> do
            mat <- do
              PhongMaterialManifest dp sp gp <- load_ root name
              PhongMaterial
                <$> retrieve texMgr (wrap,flt,cmpf,baseLvl,maxLvl) dp
                <*> retrieve texMgr (wrap,flt,cmpf,baseLvl,maxLvl) sp
                <*> retrieve texMgr (wrap,flt,cmpf,baseLvl,maxLvl) gp
            liftIO . writeIORef ref $ insert name mat mp
            return mat
      release_ ref name = liftIO . modifyIORef ref $ delete name

type PhongMaterialManager = Manager (Texture2DManager,Wrap,Filter,Maybe CompareFunc,Natural,Natural) PhongMaterial

phong :: (MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
      => m (Program' PhongMaterial)
phong = do
    prog <- buildProgram phongVS Nothing Nothing phongFS
    return (prog,semantics)
  where
    semantics mat = do
      uniform phongDiffMapSem $= (diffuseMap mat,0 :: Natural)
      uniform phongSpecMapSem $= (specularMap mat,1 :: Natural)
      uniform phongGlossMapSem $= (glossMap mat,2 :: Natural)

phongDiffMapSem :: Natural
phongDiffMapSem = 128

phongSpecMapSem :: Natural
phongSpecMapSem = 129

phongGlossMapSem :: Natural
phongGlossMapSem = 130

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
  , "  uint shadowLOD;"
  , "  uint shadowmapIndex;"
  , " };"
    -- shadows
  , declUniform lowShadowmapsSem "samplerCubeArray lowShadowmaps"
  , declUniform mediumShadowmapsSem "samplerCubeArray mediumShadowmaps"
  , declUniform highShadowmapsSem "samplerCubeArray highShadowmaps"

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
  , "    uint shadowLOD = omniBuffer.ligs[i].shadowLOD;"
  , "    uint shadowmapIndex = omniBuffer.ligs[i].shadowmapIndex;"
  , "    vec3 ligToVertex = omniBuffer.ligs[i].pos - vco;"
  , "    vec3 ligDir = normalize(ligToVertex);"
  , "    vec3 r = normalize(reflect(-ligDir,vno));"
  , "    vec3 spec = pow(max(0.,dot(r,v)), 1. + phongGloss * 1000.) * ligCol * phongSpec;"
  , "    vec3 diff = max(0.,dot(vno,ligDir)) * (ligCol - spec) * phongDiff;"
  , "    float atten = ligPow / (pow(1. + length(ligToVertex)/ligRad,2.));"
  , "    omni += atten * (diff + spec);"
  , "  }"

  , "  frag = clamp(vec4(ambient + omni,1.), vec4(0.,0.,0.,0.), vec4(1.,1.,1.,1.));"
  , "}"
  ]

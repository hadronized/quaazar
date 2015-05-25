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

import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Aeson
import Quaazar.Render.GL.Shader ( buildProgram )
import Quaazar.Render.GL.Texture ( Texture2D, Unit(..) )
import Quaazar.Render.Semantics
import Quaazar.System.Loader
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

getPhong :: (MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
         => m (Program' PhongMaterial)
getPhong = do
    prog <- buildProgram phongVS Nothing Nothing phongFS
    return (prog,semantics)
  where
    semantics mat = do
      toUniform (extendUniformSem PhongDiffMapSem) $= (diffuseMap mat,Unit 0)
      toUniform (extendUniformSem PhongSpecMapSem) $= (specularMap mat,Unit 1)
      toUniform (extendUniformSem PhongGlossMapSem) $= (glossMap mat,Unit 2)

data PhongMaterialSem
  = PhongDiffMapSem
  | PhongSpecMapSem
  | PhongGlossMapSem
    deriving (Enum,Eq,Ord,Show)

phongVS :: String
phongVS = unlines
  [
    "#version 430 core"
  , "#extension AMD_vertex_shader_layer : require"

  , "layout (location = 0) in vec3 co;"
  , "layout (location = 1) in vec3 no;"
  , "layout (location = 2) in vec2 uv;"

  , declUniform CamProjViewSem "mat4 projView"
  , declUniform ModelSem "mat4 model"
  , declUniform LayerSem "int layer"

  , "out vec3 vco;"
  , "out vec3 vno;"
  , "out vec2 vuv;"

  , "void main() {"
  , "  vco = (model * vec4(co,1.)).xyz;"
  , "  vno = normalize((transpose(inverse(model)) * vec4(no,1.)).xyz);"
  , "  vuv = uv;"
  , "  gl_Layer = layer;"
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

  , declUniform EyeSem "vec3 eye"
  , declUniform (extendUniformSem PhongDiffMapSem) "sampler2D phongDiffMap"
  , declUniform (extendUniformSem PhongSpecMapSem) "sampler2D phongSpecMap"
  , declUniform (extendUniformSem PhongGlossMapSem) "sampler2D phongGlossMap"
    -- ambient lighting
  , declUniform LigAmbColSem "vec3 ligAmbCol"
  , declUniform LigAmbPowSem "float ligAmbPow"
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
  , declUniform LowShadowmapsSem "samplerCubeArray lowShadowmaps"
  , declUniform MediumShadowmapsSem "samplerCubeArray mediumShadowmaps"
  , declUniform HighShadowmapsSem "samplerCubeArray highShadowmaps"

  , declUniformBlock ligOmniSSBOBP "OmniBuffer { Omni ligs[]; } omniBuffer"
  , declUniform LigOmniNbSem "uint ligOmniNb"

  , "out vec4 frag;"

  , "float sampleShadowmap(uint lod, uint index, vec3 ligDir) {"
  , "  switch (lod) {"
  , "    case 1u:"
  , "      return texture(lowShadowmaps, vec4(ligDir, float(index))).r;"
  
  , "    case 2u:"
  , "      return texture(mediumShadowmaps, vec4(ligDir, float(index))).r;"

  , "    case 3u:"
  , "      return texture(highShadowmaps, vec4(ligDir, float(index))).r;"

  , "    default:"
  , "      return 1.;"
  , "  }"
  , "}"

  , "float computeShadow(uint lod, uint index, vec3 ligDir, float ligRad, float distToLight) {"
  , "  float shadowDist = sampleShadowmap(lod, index, ligDir) * ligRad;"
  , "  float shadowBias = 0.01;"
  , "  return shadowDist + shadowBias >= distToLight ? 1. : 0.;"
  , "}"

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
         -- lighting
  , "    vec3 ligCol = omniBuffer.ligs[i].col;"
  , "    float ligPow = omniBuffer.ligs[i].pow;"
  , "    float ligRad = omniBuffer.ligs[i].rad;"
  , "    uint shadowLOD = omniBuffer.ligs[i].shadowLOD;"
  , "    uint shadowmapIndex = omniBuffer.ligs[i].shadowmapIndex;"
  , "    vec3 ligToVertex = omniBuffer.ligs[i].pos - vco;"
  , "    vec3 ligDir = normalize(ligToVertex);"
  , "    float distToLight = length(ligToVertex);"
  , "    vec3 r = normalize(reflect(-ligDir,vno));"
  , "    vec3 spec = pow(max(0.,dot(r,v)), 1. + phongGloss * 512.) * ligCol * phongSpec;"
  , "    vec3 diff = max(0.,dot(vno,ligDir)) * (ligCol - spec) * phongDiff;"
  , "    float atten = ligPow / (pow(1. + distToLight/ligRad,2.));"
         -- shadowing
  , "    float shadow = shadowLOD == 0u ? 1. : computeShadow(shadowLOD, shadowmapIndex, -ligDir, ligRad, distToLight);"
         -- lighting * shadowing
  , "    omni += shadow * atten * (diff + spec);"
  , "  }"

  , "  frag = clamp(vec4(ambient + omni,1.), vec4(0.,0.,0.,0.), vec4(1.,1.,1.,1.));"
  , "}"
  ]

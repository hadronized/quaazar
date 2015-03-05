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

import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO )
import Quaazar.Core.Albedo ( Albedo )
import Quaazar.Render.GL.Shader ( (@=), uniform )
import Quaazar.Render.GLSL
import Quaazar.Render.Shader
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

data PhongMaterial = PhongMaterial {
    _phongDiffAlb :: Albedo
  , _phongSpecAlb :: Albedo
  , _phongShn     :: Float
  }

makeLenses ''PhongMaterial

phong :: (MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
      => m (GPUProgram PhongMaterial)
phong = gpuProgram phongVS Nothing phongFS $ \(PhongMaterial diffAlb specAlb shn) -> do
    diffAlbSem @= diffAlb
    specAlbSem @= specAlb
    shnSem @= shn
  where
    diffAlbSem = uniform (fromIntegral phongDiffAlbSem)
    specAlbSem = uniform (fromIntegral phongSpecAlbSem)
    shnSem = uniform (fromIntegral phongShnSem)

phongDiffAlbSem :: Int
phongDiffAlbSem = 10

phongSpecAlbSem :: Int
phongSpecAlbSem = 11

phongShnSem :: Int
phongShnSem = 12

phongVS :: String
phongVS = unlines
  [
    "#version 430 core"

  , "layout (location = 0) in vec3 co;"
  , "layout (location = 1) in vec3 no;"

  , declUniform camProjViewSem "mat4 projView"
  , declUniform modelSem "mat4 model"

  , "out vec3 vco;"
  , "out vec3 vno;"

  , "void main() {"
  , "  vco = (model * vec4(co,1.)).xyz;"
  , "  vno = normalize((transpose(inverse(model)) * vec4(no,1.)).xyz);"
  , "  gl_Position = projView * vec4(vco,1.);"
  , "}"
  ]

phongFS :: String
phongFS = unlines
  [
    "#version 430 core"

  , "in vec3 vco;"
  , "in vec3 vno;"

  , declUniform eyeSem "vec3 eye"
  , declUniform phongDiffAlbSem "vec3 phongDiffAlb"
  , declUniform phongSpecAlbSem "vec3 phongSpecAlb"
  , declUniform phongShnSem "float phongShn"
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
  , "  vec3 v = normalize(eye - vco);"

    -- ambient lighting
  , "  vec3 ambient = ligAmbCol * phongDiffAlb * ligAmbPow;"

    -- omni lights
  , "  vec3 omni = vec3(0.,0.,0.);"
  , "  for (uint i = 0u; i < ligOmniNb; ++i) {"
  , "    vec3 ligCol = omniBuffer.ligs[i].col;"
  , "    float ligPow = omniBuffer.ligs[i].pow;"
  , "    float ligRad = omniBuffer.ligs[i].rad;"
  , "    vec3 ligToVertex = omniBuffer.ligs[i].pos - vco;"
  , "    vec3 ligDir = normalize(ligToVertex);"
  , "    vec3 r = normalize(reflect(-ligDir,vno));"
  , "    vec3 diff = max(0.,dot(vno,ligDir)) * ligCol * phongDiffAlb;"
  , "    vec3 spec = pow(max(0.,dot(r,v)),phongShn) * ligCol * phongSpecAlb;"
  , "    float atten = ligPow / (pow(1. + length(ligToVertex)/ligRad,2.));"
  , "    omni += atten * (diff + spec);"
  , "  }"

  , "  frag = vec4(ambient + omni,1.);"
  , "}"
  ]

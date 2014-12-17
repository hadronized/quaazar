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

module Photon.Interface.Game.Shaders where

lightVS :: String
lightVS = unlines
  [
    "#version 330 core"

  , "layout (location = 0) in vec3 co;"
  , "layout (location = 1) in vec3 no;"
  , "layout uniform mat4 projView;"
  , "layout uniform mat4 model;"

  , "out vec3 vco;"
  , "out vec3 vno;"

  , "void main() {"
  , "  vco = (model * vec4(co,1.)).xyz;"
  , "  vno = (transpose(inverse(model)) * vec4(no,1.)).xyz;"
  , "  gl_Position = projView * vec4(vco,1.);"
  , "}"
  ]

lightFS :: String
lightFS = unlines
  [
    "#version 330 core"

  , "in vec3 vco;"
  , "in vec3 vno;"

  , "layout uniform vec3 eye;"
  , "layout uniform vec3 forward;"
  , "layout uniform vec3 matDiffAlb;"
  , "layout uniform vec3 matSpecAlb;"
  , "layout uniform float matShn;"
  , "layout uniform vec3 ligPos;"
  , "layout uniform vec3 ligCol;"
  , "layout uniform float ligPow;"
  , "layout uniform float ligRad;"
  --, "layout (location = " ++ show lightProjViewSem ++ ") uniform mat4 ligProjView;"
  --, "layout (location = 2) uniform samplerCube shadowmap;"

  , "out vec4 frag;"

  , "void main() {"
  , "  vec3 ligToVertex = ligPos - vco;"
  , "  vec3 ligDir = normalize(ligToVertex);"
  , "  vec3 v = normalize(eye - vco);"
  , "  vec3 r = normalize(reflect(-ligDir,vno));"

    -- lighting
  , "  vec3 diff = max(0.,dot(vno,ligDir)) * ligCol * matDiffAlb;"
  , "  vec3 spec = pow(max(0.,dot(r,v)),matShn) * ligCol * matSpecAlb;"
  , "  float atten = ligPow / (pow(1. + length(ligToVertex)/ligRad,2.));"
  , "  vec3 illum = atten * (diff + spec);"

    -- shadows
  {-
  , "  float ligDepth = texture(shadowmap, normalize(forward)).r;"
  , "  float shadow = 1.;"
  
  , "  if (gl_FragCoord.z 
  -}

    -- final color
  , "  frag = vec4(illum,1.);"
  , "}"
  ]

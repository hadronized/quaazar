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

module Photon.Interface.Shaders where

emptyFS :: String
emptyFS = unlines
  [
    "#version 330 core"
  , "void main() {}"
  ]

--------------------------------------------------------------------------------
-- Lighting.
--
-- The lighting shader is used to render objects via lights.
--
-- TODO: for now, we only support omnidirectional lights.
lightVS :: String
lightVS = unlines
  [
    "#version 330 core"

  , "layout (location = 0) in vec3 co;"
  , "layout (location = 1) in vec3 no;"

  , "uniform mat4 projView;"
  , "uniform mat4 model;"

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

  , "uniform vec3 eye;"
  , "uniform vec3 forward;"
  , "uniform vec3 matDiffAlb;"
  , "uniform vec3 matSpecAlb;"
  , "uniform float matShn;"
  , "uniform vec3 ligPos;"
  , "uniform vec3 ligCol;"
  , "uniform float ligPow;"
  , "uniform float ligRad;"
  , "uniform samplerCube ligDepthmap;"

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
  , "  vec3 depthDir = -(vco - ligPos);"
  , "  float dist = length(depthDir);"
  , "  float ligDistance = texture(ligDepthmap, depthDir).r * ligRad;"
  , "  float shadow = 1.;"

  , "  if (ligDistance < dist) {"
  , "    shadow = 0.;"
  , "  }"

    -- final color
  , "  frag = vec4(illum,1.) * shadow;"
  , "}"
  ]

--------------------------------------------------------------------------------
-- Shadows
lightCubeDepthmapVS :: String
lightCubeDepthmapVS = unlines
  [
    "#version 330 core"

  , "layout (location = 0) in vec3 co;"
  , "layout (location = 1) in vec3 no;"

  , "uniform mat4 model;"

  , "void main() {"
  , "  gl_Position = model * vec4(co,1.);"
  , "}"
  ]

-- The geometry shader is used because we’re doing a layered rendering in order
-- to generate the whole cube depthmap in one pass. Each primitive (i.e.
-- triangle) gets duplicate 6 times; one time per cubemap face.
lightCubeDepthmapGS :: String
lightCubeDepthmapGS = unlines
  [
    "#version 330 core"

  , "layout (triangles) in;"
  , "layout (triangle_strip, max_vertices = 18) out;"

  , "out vec3 gco;"

  , "uniform mat4 ligProjViews[6];" -- 6 views

  , "void main() {"
  , "  for (int i = 0; i < 6; ++i) {"
  , "    for (int j = 0; j < 3; ++j) {"
  , "      gl_Layer = 5;"
  , "      gco = gl_in[j].gl_Position.xyz;"
  , "      gl_Position = ligProjViews[i] * gl_in[j].gl_Position;"
  , "      EmitVertex();"
  , "    }"
  , "    EndPrimitive();"
  , "  }"
  , "}"
  ]

lightCubeDepthmapFS :: String
lightCubeDepthmapFS = unlines
  [
    "#version 330 core"

  , "in vec3 gco;"
  , "out float outDistance;"

  , "uniform vec3 ligPos;"
  , "uniform float ligIRad;"

  , "void main() {"
  , "  outDistance = distance(gco, ligPos) * ligIRad;"
  , "}"
  ]

--------------------------------------------------------------------------------
-- Accumulation.
--
-- The accumulation program is used to blend a lighting image into the
-- accumulation buffer.
accumVS :: String
accumVS = unlines
  [
    "#version 330 core"
  , "vec2[4] v = vec2[]("
  , "    vec2(-1,  1)"
  , "  , vec2( 1,  1)"
  , "  , vec2(-1, -1)"
  , "  , vec2( 1, -1)"
  , "  );"
  , "void main() {"
  , "  gl_Position = vec4(v[gl_VertexID], 0., 1.);"
  , "}"
  ]

accumFS :: String
accumFS = unlines
  [
    "#version 330 core"
  , "out vec4 frag;"
  , "uniform sampler2D source;"
  , "void main() {"
  , "  frag = texelFetch(source, ivec2(gl_FragCoord.xy), 0);"
  , "}"
  ]

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
  , "uniform mat4 lightProjViewSem;"
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
  {-
  , "  float ligDepth = texture(shadowmap, normalize(forward)).r;"
  , "  float shadow = 1.;"

  , "  if (gl_FragCoord.z
  -}

    -- final color
  , "  frag = vec4(illum,1.);"
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
  , "uniform vec3 ligPos;"

  , "void main() {"
  , "  gl_Position = model * vec4(co,1.) - vec4(ligPos,1.);"
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

    -- Projection matrices (one per cubemap face).
  , "const mat4[6] faceProjection = mat4[]("
  , "    mat4(vec4(TODO),vec4(TODO),vec4(TODO),vec4(TODO))"
  , "  , mat4(vec4(TODO),vec4(TODO),vec4(TODO),vec4(TODO))"
  , "  , mat4(vec4(TODO),vec4(TODO),vec4(TODO),vec4(TODO))"
  , "  , mat4(vec4(TODO),vec4(TODO),vec4(TODO),vec4(TODO))"
  , "  );"

  , "void main() {"
  , "  for (int i = 0; i < 6; ++i) {"
  , "    for (int j = 0; j < 3; ++j) {"
  , "      gl_LayerID = i;"
  , "      gl_Position = faceProjection[i] * gl_in.gl_Position;"
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

  , "out vec4 frag;"

  , "void main() {"
  , "  frag = vec4(gl_FragDepth);"
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

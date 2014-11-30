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

module Photon.Render.OpenGL.Forward.Renderer where

import Control.Applicative
import Control.Lens hiding ( view )
import Control.Monad
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Trans.Either ( runEitherT )
import Control.Monad.Trans.Maybe ( runMaybeT )
import Data.Bits ( (.|.) )
import Foreign.Ptr ( nullPtr )
import Linear
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Photon.Core.Color
import Photon.Core.Entity
import Photon.Core.Light ( Light(..), getLightProperties, ligColor
                         , ligPower, ligRadius, ligCastShadows )
import Photon.Core.Material ( Material(..), unAlbedo )
import Photon.Core.Scene
import Photon.Render.OpenGL.Forward.Cache
import Photon.Render.OpenGL.Forward.Camera
import Photon.Render.OpenGL.Forward.Entity
import Photon.Render.OpenGL.Forward.Frame
import Photon.Render.OpenGL.Forward.Log
import Photon.Render.OpenGL.Forward.Material
import Photon.Render.OpenGL.Forward.Mesh
import Photon.Render.OpenGL.Framebuffer
import Photon.Render.OpenGL.Offscreen
import Photon.Render.OpenGL.Primitive
import Photon.Render.OpenGL.Shader
import Photon.Render.OpenGL.Texture
import Photon.Render.OpenGL.VertexArray
import Photon.Render.Renderer
import Photon.Utils.Log
import qualified Data.Vector as V ( forM_ )

data ForwardRenderer = ForwardRenderer {
    -----------------------------------------------------------------------
    -- Scene part.
    --
    -- The scene part gathers object used to render the scene. Once the
    -- scene is completely rendered, it’s accumulated (see the Accumulation
    -- part).
    -----------------------------------------------------------------------
    -- |Scene cache. This is used to efficiently represent the scene.
    _frSceneCache            :: Cache
    -- |Offscreen used to render the scene.
  , _frSceneOffscreen        :: Offscreen 
    -- |Lighting shader.
  , _frSceneShader           :: Shader
    -- |Uniforms used to customize the lighting shader.
  , _frSceneUniforms         :: SceneUniforms
    -- |Shadowmap offscreen. This offscreen is used to generate the
    -- shadowmap of each light.
  , _frSceneShadowOffscreen  :: Offscreen
    -----------------------------------------------------------------------
    -- Accumulation part.
    --
    -- Accumulation is the fact of rendering everything
    -- in a buffer in order to blend frames.
    --
    -- Note: the accumulation part uses a few objects from the Display
    -- part.
    -----------------------------------------------------------------------
    -- |Accumulation offscreen.
  , _frAccumOffscreen        :: Offscreen
    -----------------------------------------------------------------------
    -- Display part.
    --
    -- This part gathers everything needed to display a frame.
    -----------------------------------------------------------------------
    -- |Shader used to display a frame.
  , _frDisplayShader         :: Shader
    -- |Empty vertex array. Only used to be able to draw an attribute-less
    -- quadrangle.
  , _frDisplayVA             :: VertexArray
  }

data SceneUniforms = SceneUniforms {
    _sceneUniEye        :: Uniform (V3 Float)
  , _sceneUniForward    :: Uniform (V3 Float)
  , _sceneUniProjView   :: Uniform (M44 Float)
  , _sceneUniInstance   :: Uniform (M44 Float)
  , _sceneUniMatDiffAlb :: Uniform (V3 Float)
  , _sceneUniMatSpecAlb :: Uniform (V3 Float)
  , _sceneUniMatShn     :: Uniform Float
  , _sceneUniLigPos     :: Uniform (V3 Float)
  , _sceneUniLigCol     :: Uniform (V3 Float)
  , _sceneUniLigPow     :: Uniform Float
  , _sceneUniLigRad     :: Uniform Float
  }

makeLenses ''ForwardRenderer
makeLenses ''SceneUniforms

forwardRenderer :: (Applicative m,MonadIO m,MonadLogger m)
                => Natural
                -> Natural
                -> V3 Float
                -> SceneRel a
                -> m (Maybe (Renderer Frame))
forwardRenderer w h clearColor r = runMaybeT $ do
    -- OpenGL init settings
    liftIO $ do
      glEnable gl_TEXTURE_CUBE_MAP_SEAMLESS

    cch <- cacheIO r
    scnoff <- sceneOffscreen w h >>= treatError
    scnshd <- sceneShader >>= treatError
    configureSceneShader scnshd
    scnuni <- getSceneUniforms scnshd
    shdwoff <- shadowOffscreen w h >>= treatError
    accoff <- accumOffscreen w h >>= treatError
    dspshd <- displayShader >>= treatError
    configureDisplayShader dspshd
    dspva <- displayVA 
    let fr = ForwardRenderer cch scnoff scnshd scnuni shdwoff accoff dspshd dspva
    return $ Renderer
      (frender clearColor fr)
      undefined
      (fdisplay fr)
      undefined
  where
    treatError :: (MonadLogger m,MonadPlus m) => Either String a -> m a
    treatError = either onError return
    onError e = err gllog e >> mzero

cacheIO :: (MonadIO m,MonadLogger m) => SceneRel a -> m Cache
cacheIO sc = do
  info gllog "generating scene cache"
  liftIO (generateCache sc)

sceneOffscreen :: (MonadIO m,MonadLogger m) => Natural -> Natural -> m (Either String Offscreen)
sceneOffscreen w h = do
  info gllog "generating scene offscreen"
  liftIO (genOffscreen w h RGBA32F RGBA (ColorAttachment 0) Depth32F DepthAttachment)

sceneShader :: (MonadIO m,MonadLogger m) => m (Either String Shader)
sceneShader = do
    info gllog "generating scene lighting shader"
    runEitherT (sequence shadersIO >>= genShader)
  where
    shadersIO = map (uncurry genShaderStage) shaders
    shaders   = [(VertexShader,omniVSSrc),(FragmentShader,omniFSSrc)]

configureSceneShader :: (MonadIO m,MonadLogger m) => Shader -> m ()
configureSceneShader sh = do
  info gllog "configuring scene shader"
  uni <- liftIO $ do
    shadowmapUni <- getUniform sh "shadowmap"
    shadowmapUni @= (1 :: Int)
    return shadowmapUni
  displayUniform uni "shadowmap"

getSceneUniforms :: (Applicative m,MonadIO m,MonadLogger m) => Shader -> m SceneUniforms
getSceneUniforms sh = do
    info gllog "getting scene uniforms"

    SceneUniforms
      <$> getUni "eye"
      <*> getUni "forward"
      <*> getUni "projView"
      <*> getUni "instance"
      <*> getUni "matDiffAlb"
      <*> getUni "matSpecAlb"
      <*> getUni "matShn"
      <*> getUni "ligPos"
      <*> getUni "ligCol"
      <*> getUni "ligPow"
      <*> getUni "ligRad"
  where
    getUni :: (MonadIO m,MonadLogger m,Uniformable a) => String -> m (Uniform a)
    getUni name = do
      uni <- liftIO (getUniform sh name)
      displayUniform uni name
      return uni

shadowOffscreen :: (MonadIO m,MonadLogger m) => Natural -> Natural -> m (Either String Offscreen)
shadowOffscreen w h = do
  info gllog "generating shadow offscreen"
  liftIO (genCubeOffscreen w h Depth32F Depth RGBA32F)

accumOffscreen :: (MonadIO m,MonadLogger m) => Natural -> Natural -> m (Either String Offscreen)
accumOffscreen w h = do
  info gllog "generating accumulation offscreen"
  liftIO (genOffscreen w h RGBA32F RGBA (ColorAttachment 0) Depth32F DepthAttachment)

displayShader :: (MonadIO m,MonadLogger m) => m (Either String Shader)
displayShader = do
    info gllog "generating display shader"
    runEitherT (sequence shadersIO >>= genShader)
  where
    shadersIO = map (uncurry genShaderStage) shaders
    shaders   = [(VertexShader,displayVSSrc),(FragmentShader,displayFSSrc)]

configureDisplayShader :: (MonadIO m,MonadLogger m) => Shader -> m ()
configureDisplayShader sh = do
    info gllog "configuring display shader"
    offtex <- liftIO (getUniform sh "offtex")
    displayUniform offtex "offtex"
    liftIO $ offtex @= (0 :: Int)

displayVA :: (MonadIO m,MonadLogger m) => m VertexArray
displayVA = do
  info gllog "generating display vertex array"
  liftIO $ do
    va <- genVertexArray
    bindVertexArray va
    unbindVertexArray
    return va

displayUniform :: (MonadLogger m) => Uniform a -> String -> m ()
displayUniform uni name =
    if loc < 0 then
      warn gllog $ "uniform '" ++ name ++ "' is disabled"
      else
        info gllog $ "uniform '" ++ name ++ "' is enabled (" ++ show loc ++ ")"
  where
    loc = uniLoc uni

frender :: V3 Float -> ForwardRenderer -> Scene IndexPath -> Frame
frender clearColor fr sc = do
    -- clear the accumulation buffer before starting
    bindFramebuffer (fr^.frAccumOffscreen.offscreenFB) Write
    glClearColor 0 0 0 1
    glClear gl_COLOR_BUFFER_BIT
    -- FIXME: we could also enable blending here, and set ONE ZERO for each light

    -- first, send camera information
    useShader (fr^.frSceneShader)
    sendCamera fcam

    -- then for each active light...
    V.forM_ (cached^.cacheLights) $ \(flig,les) -> do
      useShader (fr^.frSceneShader)
      sendLight flig

      -- TODO: material blending props here
      forM_ les $ \le -> do
        -- clear the shadow offscreen
        bindFramebuffer (fr^.frSceneShadowOffscreen.offscreenFB) Write
        glClearColor 1 1 1 1
        glClear (gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT)

        -- if the light casts shadows, we can modify the shadow map
        -- TODO: this can be preprocessed while generating the cache
        when (getLightProperties flig ^. ligCastShadows) $ do
          return ()

        -- prepare the render of the scene
        useShader (fr^.frSceneShader)
        bindFramebuffer (fr^.frSceneOffscreen.offscreenFB) Write
        bindTextureAt (fr^.frSceneShadowOffscreen.offscreenTex) 0

        -- this block ensures we leave the scene framebuffer clean and that
        -- no blending will occur during the scene render
        glEnable gl_DEPTH_TEST
        glDisable gl_BLEND
        glClearColor cr cb cg 1
        glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

        sendLightEntity le

        -- render the cache’s objects
        V.forM_ (cached^.cacheObjects) $ \(mat,mshs) -> do
          -- send the material
          sendMaterial mat
          -- then proceed to the render of all concerned meshes
          V.forM_ mshs $ \(msh,insts) -> do
            -- prepare the base mesh
            withMesh msh $ \renderMesh -> do
              -- send all instances entities, and render them
              -- TODO: geometry instancing
              forM_ insts (\e -> sendInstanceEntity e >> renderMesh)

        -- prepare the accumulation (no depth test, additive blending)
        glDisable gl_DEPTH_TEST
        glEnable gl_BLEND
        glBlendFunc gl_ONE gl_ONE

        -- bind the lit scene texture and the accum framebuffer
        bindTextureAt (fr^.frSceneOffscreen.offscreenTex) 0
        bindFramebuffer (fr^.frAccumOffscreen.offscreenFB) Write

        -- accumulate
        useShader (fr^.frDisplayShader)
        bindVertexArray (fr^.frDisplayVA)
        glDrawArrays gl_TRIANGLE_STRIP 0 4

    return (fr^.frAccumOffscreen.offscreenTex)
  where
    V3 cr cg cb = fmap realToFrac clearColor
    cached = cache sc (fr^.frSceneCache)
    cam = sc^.camera
    fcam = fr^.frSceneCache.cacheCamera
    view = cameraTransform $ cam & entityPosition %~ negate
    sendCamera (FCamera proj) = do
        fr^.frSceneUniforms.sceneUniEye @= cam^.entityPosition
        fr^.frSceneUniforms.sceneUniProjView @= proj !*! view
    sendLight (Omni li) = do
        fr^.frSceneUniforms.sceneUniLigCol @= unColor (li^.ligColor)
        fr^.frSceneUniforms.sceneUniLigPow @= li^.ligPower
        fr^.frSceneUniforms.sceneUniLigRad @= li^.ligRadius
    sendLightEntity e =
        -- TODO: we’re using entity’s position only since we only have omit
        -- lights yet
        fr^.frSceneUniforms.sceneUniLigPos @= e^.entityPosition
    sendMaterial mat = do
        fr^.frSceneUniforms.sceneUniMatDiffAlb @= unAlbedo dalb
        fr^.frSceneUniforms.sceneUniMatSpecAlb @= unAlbedo salb
        fr^.frSceneUniforms.sceneUniMatShn @= shn
      where
        Material dalb salb shn = unFMaterial mat
    sendInstanceEntity e = do
        -- TODO: support scale matrix
        fr^.frSceneUniforms.sceneUniInstance @= entityTransform e

fdisplay :: ForwardRenderer -> Frame -> IO ()
fdisplay fr frame = do
  source <- frame

  unbindFramebuffer Write
  glClearColor 0 0 0 0
  glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
  glDisable gl_DEPTH_TEST
  glEnable gl_BLEND
  glBlendFunc gl_ONE gl_ONE

  useShader (fr^.frDisplayShader)
  bindTextureAt source 0
  bindVertexArray (fr^.frDisplayVA)
  
  glDrawArrays gl_TRIANGLE_STRIP 0 4

  -- TODO: maybe we can drop those lines
  unbindVertexArray
  unbindTexture source 

withMesh :: FMesh -> (IO () -> IO ()) -> IO ()
withMesh msh f = do
    bindVertexArray (msh^.fmeshVAO)
    f (glDrawElements (fromPrimitive $ msh^.fmeshPrim) vnb gl_UNSIGNED_INT nullPtr)
  where
    vnb = fromIntegral (msh^.fmeshVertNB)

omniVSSrc :: String
omniVSSrc = unlines
  [
    "#version 330 core"

  , "layout (location = 0) in vec3 co;"
  , "layout (location = 1) in vec3 no;"
  , "uniform mat4 projView;"
  , "uniform mat4 instance;"

  , "out vec3 vco;"
  , "out vec3 vno;"

  , "void main() {"
  , "  vco = (instance * vec4(co,1.)).xyz;"
  , "  vno = (transpose(inverse(instance)) * vec4(no,1.)).xyz;"
  , "  gl_Position = projView * vec4(vco,1.);"
  , "}"
  ]

omniFSSrc :: String
omniFSSrc = unlines
  [
    "#version 330 core"

  , "in vec3 vco;"
  , "in vec3 vno;"

  , "uniform samplerCube shadowmap;"
  , "uniform vec3 eye;"
  , "uniform vec3 forward;"
  , "uniform vec3 matDiffAlb;"
  , "uniform vec3 matSpecAlb;"
  , "uniform float matShn;"
  , "uniform vec3 ligPos;"
  , "uniform vec3 ligCol;"
  , "uniform float ligPow;"
  , "uniform float ligRad;"
  , "uniform mat4 ligProjView;"

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
  , "  float ligDepth = texture(shadowmap, normalize(forward)).r;"
  , "  float shadow = 1.;"
  
  , "  if (gl_FragCoord.z 

    -- final color
  , "  frag = vec4(illum,1.) * shadow;"
  , "}"
  ]

displayVSSrc :: String
displayVSSrc = unlines
  [
    "#version 330 core"

  , "out vec2 uv;"

  , "vec2[4] v = vec2[]("
  , "    vec2(-1,  1)"
  , "  , vec2( 1,  1)"
  , "  , vec2(-1, -1)"
  , "  , vec2( 1, -1)"
  , "  );"

  , "void main() {"
  , "  uv = (v[gl_VertexID] + 1) * 0.5;"
  , "  gl_Position = vec4(v[gl_VertexID], 0., 1.);"
  , "}"
  ]

displayFSSrc :: String
displayFSSrc = unlines
  [
    "#version 330 core"

  , "in vec2 uv;"

  , "out vec4 frag;"

  , "uniform sampler2D offtex;"

  , "void main() {"
  , "  frag = texture(offtex, uv);"
  , "}"
  ]

depthmapVSSrc :: String
depthmapVSSrc = unlines
  [
    "#version 330 core"

  , "layout (location = 0) in vec3 co;"
  , "layout (location = 1) in vec3 no;"

  , "void main() {"
  , "  frag = texture(offtex, uv);"
  , "}"
  ]

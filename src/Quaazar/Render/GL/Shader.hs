-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar.Render.GL.Shader where

import Control.Applicative
import Control.Monad ( unless )
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Error.Class ( MonadError(..) )
import Data.Foldable ( traverse_ )
import Data.Int ( Int32 )
import Data.Word ( Word32 )
import Foreign.C.String ( peekCString, withCString )
import Foreign.Marshal ( alloca )
import Foreign.Marshal.Array ( allocaArray, withArrayLen )
import Foreign.Marshal.Utils ( fromBool, with )
import Foreign.Ptr ( castPtr, nullPtr )
import Foreign.Storable ( peek )
import Linear
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Geometry.Position ( Position(unPosition) )
import Quaazar.Render.GL.GLObject
import Quaazar.Render.GL.Log ( gllog )
import Quaazar.Render.GL.Texture ( IsTexture, Unit, bindTextureAt )
import Quaazar.Scene.Color ( Color(unColor) )
import Quaazar.Utils.Log

genericGenShader :: (MonadScoped IO m)
                 => GLenum -- ^ shader type
                 -> (GLuint -> a) -- ^ Haskell wrapper
                 -> m a
genericGenShader shaderType wrapper = do
  s <- liftBase $ glCreateShader shaderType
  scoped $ glDeleteShader s
  return $ wrapper s

--------------------------------------------------------------------------------
-- Shader stages
newtype VertexShader = VertexShader { unVertexShader :: GLuint } deriving (Eq,Show)

instance GLObject VertexShader where
  genObject = genericGenShader gl_VERTEX_SHADER VertexShader

newtype TessCtrlShader = TessCtrlShader { unTessCtrlShader :: GLuint } deriving (Eq,Show)

instance GLObject TessCtrlShader where
  genObject = genericGenShader gl_TESS_CONTROL_SHADER TessCtrlShader

newtype TessEvalShader = TessEvalShader { unTessEvalShader :: GLuint } deriving (Eq,Show)

instance GLObject TessEvalShader where
  genObject = genericGenShader gl_TESS_EVALUATION_SHADER TessEvalShader

newtype GeometryShader = GeometryShader { unGeometryShader :: GLuint } deriving (Eq,Show)

instance GLObject GeometryShader where
  genObject = genericGenShader gl_GEOMETRY_SHADER GeometryShader

newtype FragmentShader = FragmentShader { unFragmentShader :: GLuint } deriving (Eq,Show)

instance GLObject FragmentShader where
  genObject = genericGenShader gl_FRAGMENT_SHADER FragmentShader

newtype ComputeShader = ComputeShader { unComputeShader :: GLuint } deriving (Eq,Show)

instance GLObject ComputeShader where
  genObject = genericGenShader gl_COMPUTE_SHADER ComputeShader

class ShaderLike s where
  shaderID :: s -> GLuint
  compile :: (MonadIO m,MonadLogger m,MonadError Log m) => s -> String -> m ()

instance ShaderLike VertexShader where
  shaderID = unVertexShader
  compile = genericCompile "vertex"

instance ShaderLike TessCtrlShader where
  shaderID = unTessCtrlShader
  compile = genericCompile "tessellation control"

instance ShaderLike TessEvalShader where
  shaderID = unTessEvalShader
  compile = genericCompile "tessellation evaluation"

instance ShaderLike GeometryShader where
  shaderID = unGeometryShader
  compile = genericCompile "geometry"

instance ShaderLike FragmentShader where
  shaderID = unFragmentShader
  compile = genericCompile "fragment"

instance ShaderLike ComputeShader where
  shaderID = unComputeShader
  compile = genericCompile "compute"

genericCompile :: (MonadIO m,MonadLogger m,MonadError Log m,ShaderLike s)
               => String
               -> s
               -> String
               -> m ()
genericCompile sname shdr src = do
    deb gllog "shader source is:"
    deb gllog src
    info gllog $ "compiling " ++ sname ++ " shader source..."
    (compiled,cl) <- liftIO $ do
      withCString src $ \cstr -> with cstr $ \pcstr -> glShaderSource sid 1 pcstr nullPtr
      glCompileShader sid
      compiled <- isCompiled sid
      ll <- clogLength sid
      cl <- clog ll sid
      return (compiled,cl)
    unless compiled $ throwLog gllog cl
  where
    sid = shaderID shdr
    isCompiled s = fmap ((==gl_TRUE) . fromIntegral) .
        alloca $ liftA2 (*>) (glGetShaderiv s gl_COMPILE_STATUS) peek
    clogLength s = fmap fromIntegral .
        alloca $ liftA2 (*>) (glGetShaderiv s gl_INFO_LOG_LENGTH) peek
    clog l s     = allocaArray l $
        liftA2 (*>) (glGetShaderInfoLog s (fromIntegral l) nullPtr) (peekCString . castPtr)

--------------------------------------------------------------------------------
-- Compute shader
dispatchCompute :: (MonadIO m) => Natural -> Natural -> Natural -> m ()
dispatchCompute wx wy wz = liftIO $ glDispatchCompute wx' wy' wz'
  where
    wx' = fromIntegral wx
    wy' = fromIntegral wy
    wz' = fromIntegral wz

--------------------------------------------------------------------------------
-- Shader program
newtype Program = Program { unProgram :: GLuint } deriving (Eq,Show)

instance GLObject Program where
  genObject = do
    p <- liftBase $ glCreateProgram
    scoped $ glDeleteProgram p
    return $ Program p

attach :: (MonadIO m,ShaderLike s) => Program -> s -> m ()
attach (Program pid) shdr = liftIO $ glAttachShader pid (shaderID shdr)

link :: (MonadIO m,MonadError Log m) => Program -> m ()
link (Program pid) = do
    (linked,cl) <- liftIO $ do
      glLinkProgram pid
      linked <- isLinked pid
      ll <- clogLength pid
      cl <- clog ll pid
      return (linked,cl)
    unless linked $ throwLog gllog cl
  where
    isLinked s   = fmap ((==gl_TRUE) . fromIntegral) .
        alloca $ liftA2 (*>) (glGetProgramiv s gl_LINK_STATUS) peek
    clogLength s = fmap fromIntegral .
        alloca $ liftA2 (*>) (glGetProgramiv s gl_INFO_LOG_LENGTH) peek
    clog l s     = allocaArray l $
        liftA2 (*>) (glGetProgramInfoLog s (fromIntegral l) nullPtr) (peekCString . castPtr)

buildProgram :: (MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
             => String
             -> Maybe (String,String)
             -> Maybe String
             -> String
             -> m Program
buildProgram vsSrc tcstesSrc gsSrc fsSrc = do
  program <- genObject
  vs :: VertexShader <- genObject
  fs :: FragmentShader <- genObject
  sequence_ [compile vs vsSrc,compile fs fsSrc]
  liftIO $ sequence_ [attach program vs,attach program fs]
  flip traverse_ tcstesSrc $ \(tcsSrc,tesSrc) -> do
    tcs :: TessCtrlShader <- genObject
    tes :: TessEvalShader <- genObject
    sequence_ [compile tcs tcsSrc,compile tes tesSrc]
    liftIO $ sequence_ [attach program tcs,attach program tes]
  case gsSrc of
    Just src -> do
      gs :: GeometryShader <- genObject
      compile gs src
      liftIO (attach program gs)
      link program
    Nothing -> link program
  return program

useProgram :: Program -> IO ()
useProgram (Program pid) = glUseProgram pid

--------------------------------------------------------------------------------
-- Uniforms
infixr 1 @=
newtype Uniform a = Uniform { (@=) :: a -> IO () }

getUniformLocation :: Program -> String -> IO GLint
getUniformLocation (Program pid) name =
  withCString name (glGetUniformLocation pid)

uniform :: (Uniformable a) => Natural -> Uniform a
uniform l = Uniform (sendUniform l')
  where
    l' = fromIntegral l

getUniform :: (Uniformable a) => Program -> String -> IO (Uniform a)
getUniform prog name = do
    l <- getUniformLocation prog name
    print . Log InfoLog gllog $ "uniform '" ++ name ++ "': " ++ show l
    if l < 0 then
      return . Uniform . const $ return ()
      else
        return . Uniform $ sendUniform l

(@?=) :: Maybe (Uniform a) -> a -> IO ()
u @?= a = traverse_ (@= a) u

unused :: (Uniformable a) => Uniform a
unused = Uniform . const $ return ()

class Uniformable a where
  sendUniform :: GLint -> a -> IO ()

instance Uniformable Bool where
  sendUniform l x = glUniform1i l (fromBool x)

instance Uniformable [Bool] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform1iv l (fromIntegral s) (castPtr p)

instance Uniformable Natural where
  sendUniform l x = glUniform1i l (fromIntegral x)

instance Uniformable Int32 where
  sendUniform l x = glUniform1i l (fromIntegral x)

instance Uniformable [Int32] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform1iv l (fromIntegral s) (castPtr p)

instance Uniformable Word32 where
  sendUniform l x = glUniform1ui l (fromIntegral x)

instance Uniformable [Word32] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform1uiv l (fromIntegral s) (castPtr p)

instance Uniformable Float where
  sendUniform l x = glUniform1f l (realToFrac x)

instance Uniformable [Float] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform1fv l (fromIntegral s) (castPtr p)

instance Uniformable (V2 Bool) where
  sendUniform l v2 = glUniform2i l x y
    where
      V2 x y = fmap fromBool v2

instance Uniformable [V2 Bool] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform2iv l (fromIntegral s) (castPtr p)

instance Uniformable (V2 Int32) where
  sendUniform l v2 = glUniform2i l x y
    where
      V2 x y = fmap fromIntegral v2

instance Uniformable [V2 Int32] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform2iv l (fromIntegral s) (castPtr p)

instance Uniformable (V2 Word32) where
  sendUniform l v2 = glUniform2ui l x y
    where
      V2 x y = fmap fromIntegral v2

instance Uniformable [V2 Word32] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform2uiv l (fromIntegral s) (castPtr p)

instance Uniformable (V2 Float) where
  sendUniform l v2 = glUniform2f l x y
    where
      V2 x y = fmap realToFrac v2

instance Uniformable [V2 Float] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform2fv l (fromIntegral s) (castPtr p)

instance Uniformable (V3 Bool) where
  sendUniform l v3 = glUniform3i l x y z
    where
      V3 x y z = fmap fromBool v3

instance Uniformable [V3 Bool] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform3iv l (fromIntegral s) (castPtr p)

instance Uniformable (V3 Int32) where
  sendUniform l v3 = glUniform3i l x y z
    where
      V3 x y z = fmap fromIntegral v3

instance Uniformable [V3 Int32] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform3iv l (fromIntegral s) (castPtr p)

instance Uniformable (V3 Word32) where
  sendUniform l v3 = glUniform3ui l x y z
    where
      V3 x y z = fmap fromIntegral v3

instance Uniformable [V3 Word32] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform3uiv l (fromIntegral s) (castPtr p)

instance Uniformable (V3 Float) where
  sendUniform l v3 = glUniform3f l x y z
    where
      V3 x y z = fmap realToFrac v3

instance Uniformable [V3 Float] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform3fv l (fromIntegral s) (castPtr p)

instance Uniformable (V4 Bool) where
  sendUniform l v4 = glUniform4i l x y z w
    where
      V4 x y z w = fmap fromBool v4

instance Uniformable [V4 Bool] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform4iv l (fromIntegral s) (castPtr p)

instance Uniformable (V4 Int32) where
  sendUniform l v4 = glUniform4i l x y z w
    where
      V4 x y z w = fmap fromIntegral v4

instance Uniformable [V4 Int32] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform4iv l (fromIntegral s) (castPtr p)

instance Uniformable (V4 Word32) where
  sendUniform l v4 = glUniform4ui l x y z w
    where
      V4 x y z w = fmap fromIntegral v4

instance Uniformable [V4 Word32] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform4uiv l (fromIntegral s) (castPtr p)

instance Uniformable (V4 Float) where
  sendUniform l v4 = glUniform4f l x y z w
    where
      V4 x y z w = fmap realToFrac v4

instance Uniformable [V4 Float] where
  sendUniform l a =
    withArrayLen a $ \s p -> glUniform4fv l (fromIntegral s) (castPtr p)

instance Uniformable (M44 Float) where
  sendUniform l m = with m (glUniformMatrix4fv l 1 (fromBool True) . castPtr)

instance Uniformable [M44 Float] where
  sendUniform l a =
    withArrayLen a $ \s p ->
      glUniformMatrix4fv l (fromIntegral s) (fromBool True) (castPtr p)

instance Uniformable Color where
  sendUniform l = sendUniform l . unColor

instance Uniformable Position where
  sendUniform l = sendUniform l . unPosition

instance (IsTexture t) => Uniformable (t,Unit) where
  sendUniform l (tex,texUnit) = do
    bindTextureAt tex texUnit
    sendUniform l (fromIntegral texUnit :: Int32)

--------------------------------------------------------------------------------
-- Semantics

-- |Semantics are used by the user to customize shaders. It basically
-- exposes all 'Uniformable' instances, but constraint them into pure code.
--
-- See '($=)' for building 'Semantics'.
newtype Semantics a = Semantics {
    -- |Update all semantics into the currently bound shader program.ã
    runSemantics :: IO a
  } deriving (Applicative,Functor,Monad)

-- |Map a semantic to its value.
($=) :: (Uniformable a) => Uniform a -> a -> Semantics ()
s $= a = Semantics $ s @= a

-- FIXME: needs a better name
-- |'Program' with its 'Semantic's.
type Program' a = (Program,a -> Semantics ())

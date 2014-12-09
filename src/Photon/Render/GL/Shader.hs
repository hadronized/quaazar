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

module Photon.Render.GL.Shader where

import Control.Applicative
import Control.Monad ( unless )
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Error.Class ( MonadError(..) )
import Data.Foldable ( traverse_ )
import Data.Word ( Word32 )
import Foreign.Concurrent
import Foreign.C.String ( peekCString, withCString )
import Foreign.Marshal ( alloca, malloc, free )
import Foreign.Marshal.Array ( allocaArray )
import Foreign.Marshal.Utils ( fromBool, with )
import Foreign.Ptr ( castPtr, nullPtr )
import Foreign.Storable ( peek, poke )
import Linear
import Graphics.Rendering.OpenGL.Raw
import Photon.Render.GL.GLObject
import Photon.Utils.Log

gllog :: LogCommitter
gllog = BackendLog "gl"

newtype Shader = Shader { unShader :: GLObject } deriving (Eq,Show)

data ShaderType
  = VertexShader
  | FragmentShader
    deriving (Eq,Show)

newtype Program = Program { unProgram :: GLObject } deriving (Eq,Show)

genShader :: (MonadIO m,MonadLogger m,MonadError String m) => ShaderType -> String -> m Shader
genShader stype src = do
    deb gllog "shader source is:"
    deb gllog src
    info gllog $ "compiling " ++ shaderType ++ " shader source..."
    (p,s,compiled,cl) <- liftIO $ do
      p <- malloc
      s <- glCreateShader (fromShaderType stype)
      poke p s
      withCString src $ \cstr -> with cstr $ \pcstr -> glShaderSource s 1 pcstr nullPtr
      glCompileShader s
      compiled <- isCompiled s
      ll <- clogLength s
      cl <- clog ll s
      return (p,s,compiled,cl)
    unless compiled $ throwError cl
    info gllog "done..."
    liftIO $ Shader . GLObject <$> newForeignPtr p (glDeleteShader s >> free p)
  where
    shaderType
        | stype == VertexShader   = "vertex"
        | stype == FragmentShader = "fragment"
        | otherwise               = "unknown"
    isCompiled s = fmap ((==gl_TRUE) . fromIntegral) .
        alloca $ liftA2 (*>) (glGetShaderiv s gl_COMPILE_STATUS) peek
    clogLength s = fmap fromIntegral .
        alloca $ liftA2 (*>) (glGetShaderiv s gl_INFO_LOG_LENGTH) peek
    clog l s     = allocaArray l $
        liftA2 (*>) (glGetShaderInfoLog s (fromIntegral l) nullPtr) (peekCString . castPtr)

fromShaderType :: ShaderType -> GLenum
fromShaderType shaderType = case shaderType of
  VertexShader   -> gl_VERTEX_SHADER
  FragmentShader -> gl_FRAGMENT_SHADER

genProgram :: (MonadIO m,MonadError String m) => [Shader] -> m Program
genProgram shaders = do
    (p,sp,linked,cl) <- liftIO $ do
      p <- malloc
      sp <- glCreateProgram
      poke p sp
      mapM_ (\shd -> withGLObject (unShader shd) $ glAttachShader sp) shaders
      glLinkProgram sp
      linked <- isLinked sp
      ll <- clogLength sp
      cl <- clog ll sp
      return (p,sp,linked,cl)
    unless linked $ throwError cl
    liftIO $ Program . GLObject <$> newForeignPtr p (glDeleteProgram sp >> free p)
  where
    isLinked s   = fmap ((==gl_TRUE) . fromIntegral) .
        alloca $ liftA2 (*>) (glGetProgramiv s gl_LINK_STATUS) peek
    clogLength s = fmap fromIntegral .
        alloca $ liftA2 (*>) (glGetProgramiv s gl_INFO_LOG_LENGTH) peek
    clog l s     = allocaArray l $
        liftA2 (*>) (glGetProgramInfoLog s (fromIntegral l) nullPtr) (peekCString . castPtr)

useProgram :: Program -> IO ()
useProgram (Program s) = withGLObject s glUseProgram

infixr 1 @=
data Uniform a = Uniform { uniLoc :: GLint, (@=) :: a -> IO () }

instance Show (Uniform a) where
  show (Uniform l _) = show l

getUniformLocation :: Program -> String -> IO GLint
getUniformLocation (Program program) name = withGLObject program (withCString name . glGetUniformLocation)

uniform :: (Uniformable a) => GLint -> Uniform a
uniform l = Uniform l (sendUniform l)

getUniform :: (Uniformable a) => Program -> String -> IO (Uniform a)
getUniform prog name = do
  l <- getUniformLocation prog name
  return $ Uniform l (sendUniform l)

(@?=) :: Maybe (Uniform a) -> a -> IO ()
u @?= a = traverse_ (@= a) u

class Uniformable a where
  sendUniform :: GLint -> a -> IO ()

instance Uniformable Int where
  sendUniform l x = glUniform1i l (fromIntegral x)

instance Uniformable Word32 where
  sendUniform l x = glUniform1ui l (fromIntegral x)

instance Uniformable Float where
  sendUniform l x = glUniform1f l (realToFrac x)

instance Uniformable (V2 Int) where
  sendUniform l v2 = glUniform2i l x y
    where
      V2 x y = fmap fromIntegral v2

instance Uniformable (V2 Word32) where
  sendUniform l v2 = glUniform2ui l x y
    where
      V2 x y = fmap fromIntegral v2

instance Uniformable (V2 Float) where
  sendUniform l v2 = glUniform2f l x y
    where
      V2 x y = fmap realToFrac v2

instance Uniformable (V3 Int) where
  sendUniform l v3 = glUniform3i l x y z
    where
      V3 x y z = fmap fromIntegral v3

instance Uniformable (V3 Word32) where
  sendUniform l v3 = glUniform3ui l x y z
    where
      V3 x y z = fmap fromIntegral v3

instance Uniformable (V3 Float) where
  sendUniform l v3 = glUniform3f l x y z
    where
      V3 x y z = fmap realToFrac v3

instance Uniformable (V4 Int) where
  sendUniform l v4 = glUniform4i l x y z w
    where
      V4 x y z w = fmap fromIntegral v4

instance Uniformable (V4 Word32) where
  sendUniform l v4 = glUniform4ui l x y z w
    where
      V4 x y z w = fmap fromIntegral v4

instance Uniformable (V4 Float) where
  sendUniform l v4 = glUniform4f l x y z w
    where
      V4 x y z w = fmap realToFrac v4

instance Uniformable (M44 Float) where
  sendUniform l m = with m (glUniformMatrix4fv l 1 (fromBool True) . castPtr)

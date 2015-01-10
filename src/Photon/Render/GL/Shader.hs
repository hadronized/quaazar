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
import Foreign.C.String ( peekCString, withCString )
<<<<<<< HEAD
import Foreign.Marshal ( alloca, malloc, free )
import Foreign.Marshal.Array ( allocaArray, withArrayLen )
=======
import Foreign.Marshal ( alloca )
import Foreign.Marshal.Array ( allocaArray )
>>>>>>> gl_explicit_deletes
import Foreign.Marshal.Utils ( fromBool, with )
import Foreign.Ptr ( castPtr, nullPtr )
import Foreign.Storable ( peek )
import Linear
import Graphics.Rendering.OpenGL.Raw
import Photon.Core.Color ( Color(unColor) )
import Photon.Core.Material ( Albedo(unAlbedo) )
import Photon.Core.Position ( Position(unPosition) )
import Photon.Render.GL.GLObject
import Photon.Render.GL.Log ( gllog )
import Photon.Utils.Log

throwError_ :: (MonadError Log m) => String -> m a
throwError_ = throwError . Log ErrorLog gllog

newtype VertexShader = VertexShader { unVertexShader :: GLuint } deriving (Eq,Show)

<<<<<<< HEAD
data ShaderType
  = VertexShader
  | GeometryShader
  | FragmentShader
    deriving (Eq,Show)
=======
instance GLObject VertexShader where
  genObject = fmap VertexShader $ glCreateShader gl_VERTEX_SHADER
  deleteObject (VertexShader s) = glDeleteShader s
>>>>>>> gl_explicit_deletes

newtype GeometryShader = GeometryShader { unGeometryShader :: GLuint } deriving (Eq,Show)

instance GLObject GeometryShader where
  genObject = fmap GeometryShader $ glCreateShader gl_GEOMETRY_SHADER
  deleteObject (GeometryShader s) = glDeleteShader s

newtype FragmentShader = FragmentShader { unFragmentShader :: GLuint } deriving (Eq,Show)

instance GLObject FragmentShader where
  genObject = fmap FragmentShader $ glCreateShader gl_GEOMETRY_SHADER
  deleteObject (FragmentShader s) = glDeleteShader s

newtype Program = Program { unProgram :: GLuint } deriving (Eq,Show)

instance GLObject Program where
  genObject = fmap Program glCreateProgram
  deleteObject (Program p) = glDeleteProgram p

class ShaderLike s where
  shaderID :: s -> GLuint
  compile :: (MonadIO m,MonadLogger m,MonadError Log m) => s -> String -> m ()

instance ShaderLike VertexShader where
  shaderID = unVertexShader
  compile = compile_ "vertex"

instance ShaderLike GeometryShader where
  shaderID = unGeometryShader
  compile = compile_ "geometry"

instance ShaderLike FragmentShader where
  shaderID = unFragmentShader
  compile = compile_ "fragment"

compile_ :: (MonadIO m,MonadLogger m,MonadError Log m,ShaderLike s)
         => String
         -> s
         -> String
         -> m ()
compile_ sname shdr src = do
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
    unless compiled $ throwError_ cl
  where
<<<<<<< HEAD
    shaderType = case stype of
        VertexShader   -> "vertex"
        FragmentShader -> "fragment"
        GeometryShader -> "geometry"
=======
    sid = shaderID shdr
>>>>>>> gl_explicit_deletes
    isCompiled s = fmap ((==gl_TRUE) . fromIntegral) .
        alloca $ liftA2 (*>) (glGetShaderiv s gl_COMPILE_STATUS) peek
    clogLength s = fmap fromIntegral .
        alloca $ liftA2 (*>) (glGetShaderiv s gl_INFO_LOG_LENGTH) peek
    clog l s     = allocaArray l $
        liftA2 (*>) (glGetShaderInfoLog s (fromIntegral l) nullPtr) (peekCString . castPtr)

<<<<<<< HEAD
fromShaderType :: ShaderType -> GLenum
fromShaderType shaderType = case shaderType of
  VertexShader   -> gl_VERTEX_SHADER
  GeometryShader -> gl_GEOMETRY_SHADER
  FragmentShader -> gl_FRAGMENT_SHADER

genProgram :: (MonadIO m,MonadError Log m) => [Shader] -> m Program
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
=======
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
>>>>>>> gl_explicit_deletes
    unless linked $ throwError_ cl
  where
    isLinked s   = fmap ((==gl_TRUE) . fromIntegral) .
        alloca $ liftA2 (*>) (glGetProgramiv s gl_LINK_STATUS) peek
    clogLength s = fmap fromIntegral .
        alloca $ liftA2 (*>) (glGetProgramiv s gl_INFO_LOG_LENGTH) peek
    clog l s     = allocaArray l $
        liftA2 (*>) (glGetProgramInfoLog s (fromIntegral l) nullPtr) (peekCString . castPtr)

buildProgram :: (Applicative m,MonadIO m,MonadLogger m,MonadError Log m)
             => String
             -> Maybe String
             -> String
             -> m Program
buildProgram vsSrc gsSrc fsSrc = do
  program <- liftIO genObject

  vs :: VertexShader <- liftIO genObject
  fs :: FragmentShader <- liftIO genObject
  sequence_ [compile vs vsSrc,compile fs fsSrc]
  liftIO $ do
    sequence_ [attach program vs,attach program fs]
    deleteObject vs
    deleteObject fs

  flip traverse_ gsSrc $ \src -> do
    gs :: VertexShader <- liftIO genObject
    compile gs src
    liftIO $ do
      attach program gs
      deleteObject gs

  link program
  return program

useProgram :: Program -> IO ()
useProgram (Program pid) = glUseProgram pid

infixr 1 @=
data Uniform a = Uniform { uniLoc :: GLint, (@=) :: a -> IO () }

instance Show (Uniform a) where
  show (Uniform l _) = show l

getUniformLocation :: Program -> String -> IO GLint
getUniformLocation (Program pid) name =
  withCString name (glGetUniformLocation pid)

uniform :: (Uniformable a) => GLint -> Uniform a
uniform l = Uniform l (sendUniform l)

getUniform :: (Uniformable a) => Program -> String -> IO (Uniform a)
getUniform prog name = do
    l <- getUniformLocation prog name
    if l < 0 then
      return $ Uniform l (const $ return ())
      else
        return $ Uniform l (sendUniform l)

(@?=) :: Maybe (Uniform a) -> a -> IO ()
u @?= a = traverse_ (@= a) u

unused :: Uniform a
unused = Uniform (-1) (const $ return ())

class Uniformable a where
  sendUniform :: GLint -> a -> IO ()

instance Uniformable Int where
  sendUniform l x = glUniform1i l (fromIntegral x)

instance Uniformable [Int] where
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

instance Uniformable (V2 Int) where
  sendUniform l v2 = glUniform2i l x y
    where
      V2 x y = fmap fromIntegral v2

instance Uniformable [V2 Int] where
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

instance Uniformable (V3 Int) where
  sendUniform l v3 = glUniform3i l x y z
    where
      V3 x y z = fmap fromIntegral v3

instance Uniformable [V3 Int] where
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

instance Uniformable (V4 Int) where
  sendUniform l v4 = glUniform4i l x y z w
    where
      V4 x y z w = fmap fromIntegral v4

instance Uniformable [V4 Int] where
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

instance Uniformable Albedo where
  sendUniform l = sendUniform l . unAlbedo

instance Uniformable Color where
  sendUniform l = sendUniform l . unColor

instance Uniformable Position where
  sendUniform l = sendUniform l . unPosition

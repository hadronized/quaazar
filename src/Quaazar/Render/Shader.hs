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

module Quaazar.Render.Shader (
    GPUProgram(useProgram, sendToProgram)
  , gpuProgram
  ) where

import Control.Applicative ( Applicative )
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Numeric.Natural ( Natural )
import Quaazar.Render.GL.Shader ( buildProgram )
import qualified Quaazar.Render.GL.Shader as GL ( Uniformable, (@=)
                                                , uniform, useProgram )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

-- |Semantics are used by the user to customize shaders. It basically
-- exposes all 'Uniformable' instances, but constraint the use to stay
-- within pure code.
--
-- See '($=)' for building 'Semantics'.
newtype Semantics a = Semantics { runSemantics :: IO a }
  deriving (Applicative,Functor,Monad)

-- |Map a semantic to its value.
($=) :: (Uniformable a) => Natural -> a -> Semantics
s $= a = Semantics $ uniform s @= a

-- |A program that lives on the GPU.
data GPUProgram a = GPUProgram {
    useProgram :: IO ()
  , sendToProgram :: a -> IO ()
  }

-- |@gpuProgram vs ts gs fs semMapper@ builds a shader program
-- that lives on the GPU.
--
-- 'vs' is a mandatory /vertex shader/. You cannot get rid of it and
-- have to provide an implementation, even trivial.
--
-- 'ts' is an optional pair of /tessellation shaders/. If you
-- don’t want tessellation, pass 'Nothing'. Otherwise, you have to
-- pass @Just (tcs,tes)@ where 'tcs' is the /tessellation control shader/
-- and 'tes' the /tessellation evaluation shader/.
--
-- 'gs' is an optional /geometry shader/.
--
-- 'fs' is the final mandatory /fragment shader/.
--
-- 'semMapper' is a value of type @a -> Semantics b@. 'b' will be
-- discarded so you can return anything you want – in practice, '()'.
-- Semantics are used to customize shaders in fancy ways. 'semMapper'
-- maps a value to semantics. You won’t be able to map fields of 'a'
-- that aren’t 'Uniformable'. See '($=)' for further details.
gpuProgram :: (MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
           => String
           -> Maybe (String,String)
           -> Maybe String
           -> String
           -> (a -> Semantics b)
           -> m (GPUProgram a)
gpuProgram vs tcstes gs fs semMapper = do
    program <- buildProgram vs tcstes gs fs
    return $ GPUProgram (GL.useProgram program) (runSemantics . semMapper)

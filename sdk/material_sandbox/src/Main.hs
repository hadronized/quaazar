{-# LANGUAGE OverloadedStrings #-}

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

import Control.Applicative
import Control.Monad ( forever, void, when )
import Control.Monad.Trans ( MonadIO(..) ) 
import Control.Monad.Trans.Either ( runEitherT )
import Control.Monad.Trans.Journal ( evalJournalT )
import Control.Concurrent ( threadDelay )
import Data.Foldable ( traverse_ )
import Data.IORef
import Quaazar
import System.FSNotify
import System.Environment ( getArgs )

import qualified Prelude ( print, putStrLn )
import Prelude hiding ( print, putStrLn )

putStrLn :: (MonadIO m) => String -> m ()
putStrLn = liftIO . Prelude.print . Log InfoLog UserLog

print :: (MonadIO m,Show a) => a -> m ()
print = putStrLn . show

dataRoot :: String
dataRoot = "data"

managers :: (MonadIO m)
         => (MeshManager,GPUMeshManager,TextureManager,GPUTextureManager,PhongMaterialManager)
managers =
  (,,,,)
    <$> manager dataRoot
    <*> manager dataRoot
    <*> manager dataRoot
    <*> manager dataRoot
    <*> manager dataRoot

main :: IO ()
main =  do
  args <- getArgs
  when (length args == 2) . void . evalJournalT . runIOScopedT . runEitherT $ do
    let [mshN,matN] = args
    putStrLn $ "running with mesh: " ++ mshN
    putStrLn $ "running with material: " ++ matN
    {-
    iniMesh <- 
    mesh <- liftIO $ newIORef
    mat <- liftIO $ newIORef
    -}
    return ()
    {-
    withManager $ \manager -> do
      void $ watchDir manager "/tmp" (const True) print
      forever $ threadDelay maxBound
    -}

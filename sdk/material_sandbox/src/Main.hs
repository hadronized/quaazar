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

import Control.Monad ( forever, void, when )
import Control.Monad.Trans ( MonadIO(..) ) 
import Control.Concurrent ( threadDelay )
import Data.Foldable ( traverse_ )
import Quaazar.Utils.Log
import System.FSNotify
import System.Environment ( getArgs )
import qualified Prelude ( print, putStrLn )
import Prelude hiding ( print, putStrLn )

putStrLn :: (MonadIO m) => String -> m ()
putStrLn = liftIO . Prelude.print . Log InfoLog UserLog

print :: (MonadIO m,Show a) => a -> m ()
print = putStrLn . show

main :: IO ()
main =  do
  args <- getArgs
  when (length args == 3)  $ do
    let [msh,shdr,mat] = args
    putStrLn $ "running with mesh: " ++ msh
    putStrLn $ "running with shader: " ++ shdr
    putStrLn $ "running with material: " ++ mat
    withManager $ \manager -> do
      void $ watchDir manager "/tmp" (const True) print
      forever $ threadDelay maxBound

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

import Control.Monad ( unless )
import Data.Maybe ( fromMaybe )
import Qzr.Init
import Qzr.New.Shader
import System.Environment ( getArgs )
import System.FilePath

import Prelude hiding ( init )

data Command
  = Init (Maybe FilePath)
  | NewShader String
    deriving (Eq,Show)

instance Read Command where
  readsPrec _ s = case words s of
      ("init":xs) -> processInit xs
      ("new":xs) -> processNew xs
      _ -> []
    where
      processInit xs = case xs of
        [path] -> [(Init (Just path),"")]
        _ -> [(Init Nothing,"")]
      processNew xs = case xs of
        ["shader",name] -> [(NewShader name,"")]
        _ -> []

dispatchCommand :: Command -> IO ()
dispatchCommand cmd = case cmd of
  Init path -> init (fromMaybe "./" path)
  NewShader name -> newShader name

main :: IO ()
main = do
  args <- getArgs
  let
    raw = reads (unwords args)
    cmd = fst (head raw)
  unless (null raw) (dispatchCommand cmd)

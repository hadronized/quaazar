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
  = Init String (Maybe FilePath)
  | NewShader String
    deriving (Eq,Show)

instance Read Command where
  readsPrec _ s = case words s of
      ("init":xs) -> processInit xs
      ("new":xs) -> processNew xs
      _ -> []
    where
      processInit xs = case xs of
        [name] -> [(Init name Nothing,"")]
        [name,path] -> [(Init name (Just path),"")]
        _ -> []
      processNew xs = case xs of
        ["shader",name] -> [(NewShader name,"")]
        _ -> []

dispatchCommand :: Command -> IO ()
dispatchCommand cmd = case cmd of
  Init name path -> init name (fromMaybe "./" path)
  NewShader name -> newShader name

main :: IO ()
main = do
  args <- getArgs
  let
    raw = reads (unwords args)
    cmd = fst (head raw)
  unless (null raw) (dispatchCommand cmd)


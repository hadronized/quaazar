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
import System.Environment ( getArgs )
import System.FilePath

import Prelude hiding ( init )

data Command
  = Init String (Maybe FilePath)
    deriving (Eq,Show)

instance Read Command where
  readsPrec _ s = case words s of
      ("init":xs) -> processInit xs
      _ -> []
    where
      processInit xs = case xs of
        [name] -> [(Init name Nothing,"")]
        [name,path] -> [(Init name (Just path),"")]
        _ -> []

dispatchCommand :: Command -> IO ()
dispatchCommand cmd = case cmd of
  Init name path -> init name (fromMaybe "./" path)

main :: IO ()
main = do
  args <- getArgs
  let
    raw = reads (unwords args)
    cmd = fst (head raw)
  unless (null raw) (dispatchCommand cmd)

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- This module adds a few stuff to "System.Directory".
----------------------------------------------------------------------------

module Quaazar.Utils.Directory (
    -- * Sorting files
    partitionFileDir
  ) where

import Quaazar.Control.Monad ( partitionM )
import System.Directory ( doesDirectoryExist, doesFileExist )

-- |Partition files into a list of *files* and a list of *directories*.
-- If a file is not a directory, it’s considered as a file.
partitionFileDir :: [FilePath] -> IO ([FilePath],[FilePath])
partitionFileDir files = do
    (f,r) <- partitionM doesFileExist files
    d <- fmap fst $ partitionM doesDirectoryExist r
    return (f,d)

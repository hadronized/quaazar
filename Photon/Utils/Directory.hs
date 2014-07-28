module Photon.Utils.Directory (
    -- * Sorting files
    partitionFileDir
  ) where

import Photon.Control.Monad ( partitionM )
import System.Directory ( doesDirectoryExist, doesFileExist )

partitionFileDir :: [FilePath] -> IO ([FilePath],[FilePath])
partitionFileDir files = do
    (f,r) <- partitionM doesFileExist files
    d <- fmap fst $ partitionM doesDirectoryExist r
    return (f,d)

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

module Qzr.Init (
    -- * Quaazar projection initialization
    init
  ) where

import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>) )
import Prelude hiding ( init )

init :: FilePath -> IO ()
init path = do
    putStrLn "creating new quaazar project"
    putStrLn $ "path location : " ++ path
    putStrLn "creating project root..."
    createDirectoryIfMissing True path
    putStrLn "creating project data root..."
    createDirectoryIfMissing True dataRoot
    putStrLn "creating project lights root..."
    createDirectoryIfMissing True $ dataRoot </> "lights"
    putStrLn "creating project materials root..."
    createDirectoryIfMissing True $ dataRoot </> "materials"
    putStrLn "creating project meshes root..."
    createDirectoryIfMissing True $ dataRoot </> "meshes"
    putStrLn "creating project textures root..."
    createDirectoryIfMissing True $ dataRoot </> "textures"
    putStrLn "creating project file source root..."
    createDirectoryIfMissing True srcRoot
    putStrLn "writing a default Main.hs..."
    writeFile (srcRoot </> "Main.hs") mainHS
  where
    dataRoot = path </> "data"
    srcRoot = path </> "src"

mainHS :: String
mainHS = unlines
  [
    replicate 79 '-'
  , "-- generated by: qzr init"
  , replicate 79 '-'
  , ""
  , "import Control.Monad ( unless )"
  , "import Control.Monad.Trans.Either ( runEitherT )"
  , "import Control.Monad.Trans.Journal ( runJournalT )"
  , "import Numeric.Natural ( Natural )"
  , "import Quaazar"
  , "import Quaazar.System.Event"
  , ""
  , "screenW,screenH :: Natural"
  , "screenW = 800"
  , "screenH = 600"
  , ""
  , "fullscreen :: Bool"
  , "fullscreen = False"
  , ""
  , "screenTitle :: String"
  , "screenTitle = \"quaazar\""
  , ""
  , "main :: IO ()"
  , "main = withQuaazar screenW screenH fullscreen screenTitle application"
  , ""
  , "application :: Window -> IO [Event] -> IO ()"
  , "application win pollEvents = go"
  , "  where"
  , "    go = do"
  , "      events <- pollEvents"
  , "      unless (hasQuit events) go"
  , "    hasQuit events ="
  , "         SystemEvent Quit `elem` events"
  , "      || KeyEvent Escape KeyReleased `elem` events"
  , "      || WindowEvent Closed `elem` events"
  ]

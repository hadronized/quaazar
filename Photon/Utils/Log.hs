{-# LANGUAGE ConstraintKinds #-}

module Photon.Utils.Log (
    -- * MonadLogger
    MonadLogger
    -- * Logs
  , Log(..)
  , LogQueue
  , LogType(..)
  , LogCommitter(..)
    -- * Logging functions
  , deb
  , info
  , warn
  , err
    -- * Sinking
  , sinkLogs
  ) where

import Control.Monad.Trans ( MonadIO )
import Control.Monad.Trans.Journal
import Data.Foldable as F ( mapM_ )
import Data.Vector ( Vector, fromList )

type MonadLogger m = (MonadJournal LogQueue m)

-- |A log.
data Log = Log LogType LogCommitter String deriving (Eq)

type LogQueue = Vector Log 

-- |Type of log.
data LogType
  = DebLog     -- ^ A debug/trace log
  | InfoLog    -- ^ An informational log
  | WarningLog -- ^ A warning log
  | ErrorLog   -- ^ An error log
    deriving (Eq)

-- |Committer of a log.
data LogCommitter
  = CoreLog           -- Core committer
  | BackendLog String -- Specific backend committer
  | UserLog           -- User committer
    deriving (Eq)

instance Show Log where
  show (Log lt lc msg) = show lt ++ " [" ++ show lc ++ "] > " ++ msg

instance Show LogType where
  show t = case t of
    DebLog     -> "deb"
    InfoLog    -> "inf"
    WarningLog -> "war"
    ErrorLog   -> "err"

instance Show LogCommitter where
  show c = case c of
    CoreLog         -> "core"
    BackendLog impl -> take 4 $ impl ++ repeat ' '
    UserLog         -> "user"

-- |Create a debug log.
deb :: (MonadLogger m) => LogCommitter -> String -> m ()
deb c = journal_ . Log DebLog c

-- |Create an informational log.
info :: (MonadLogger m) => LogCommitter -> String -> m ()
info c = journal_ . Log InfoLog c

-- |Create a warning log.
warn :: (MonadLogger m) => LogCommitter -> String -> m ()
warn c = journal_ . Log WarningLog c

-- |Create an error log.
err :: (MonadLogger m) => LogCommitter -> String -> m ()
err c = journal_ . Log ErrorLog c

journal_ :: (MonadLogger m) => Log -> m ()
journal_ = journal . fromList . discardNewlines

discardNewlines :: Log -> [Log]
discardNewlines (Log lt lc msg) = map (Log lt lc) (lines msg)

sinkLogs :: (MonadLogger m,MonadIO m) => m ()
sinkLogs = sink (F.mapM_ print)

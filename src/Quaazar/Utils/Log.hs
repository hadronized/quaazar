{-# LANGUAGE ConstraintKinds #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Logging interface module.
--
-- Logging uses "Control.Monad.Journal" (hence the 'MonadJournal'
-- typeclass). However, this module exposes a few combinators that might
-- help you a lot:
--
--   - 'deb' is used to log debug messages ;
--   - 'info' is used to log informational messages ;
--   - 'warn' is used to log warnings ;
--   - 'err' is used to log errors.
--
-- You also have a pretty neat 'sinkLogs', which is a default 'sink'
-- implementation for 'MonadJournal'. Of course, you can use you're own if
-- you want to.
--
-- If you intend to handle the logs yourself, you have to understand what
-- logs look like. Logs gather three things:
--
--   - a committer ('LogCommitter'), which is the entity responsible of the
--     log ;
--   - a type ('LogType'), which is the type of the log (debug,
--     informational, warning and so on and so forth) ;
--   - a 'String' message.
--
-- Logs are gathered in a 'Traversable' queue ('LogQueue'). Feel free to
-- traverse it and handle logs anyway you want then!
----------------------------------------------------------------------------

module Quaazar.Utils.Log (
    -- * MonadLogger
    MonadLogger
    -- * Logs
  , Log(..)
  , LogQueue
  , LogType(..)
  , LogCommitter(..)
    -- * Logging functions
  , log_
  , deb
  , info
  , warn
  , err
  , throwLog
    -- * Sinking
  , sinkLogs
  ) where

import Control.Monad.Error.Class ( MonadError(..) )
import Control.Monad.Trans ( MonadIO )
import Control.Monad.Trans.Journal
import Data.Foldable ( traverse_ )
import Data.Vector ( Vector, fromList )

-- |Monad used to log.
type MonadLogger m = (MonadJournal LogQueue m)

-- |A plain log with meta-information.
data Log = Log LogType LogCommitter String deriving (Eq)

-- FIXME: use DList instead
-- |Logs queue.
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
  | RendererLog         -- Renderer committer
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
    RendererLog     -> "rndr"
    BackendLog impl -> take 4 $ impl ++ repeat ' '
    UserLog         -> "user"

-- |Create a log.
log_ :: (MonadLogger m) => LogType -> LogCommitter -> String -> m ()
log_ t c m = journal_ (Log t c m)

-- |Create a debug log.
deb :: (MonadLogger m) => LogCommitter -> String -> m ()
deb = log_ DebLog

-- |Create an informational log.
info :: (MonadLogger m) => LogCommitter -> String -> m ()
info = log_ InfoLog

-- |Create a warning log.
warn :: (MonadLogger m) => LogCommitter -> String -> m ()
warn = log_ WarningLog

-- |Create an error log.
err :: (MonadLogger m) => LogCommitter -> String -> m ()
err = log_ ErrorLog

-- |Alternative way of outputting logs, through 'MonadError'.
throwLog :: (MonadError Log m) => LogCommitter -> String -> m a
throwLog lc msg = throwError (Log ErrorLog lc msg)

journal_ :: (MonadLogger m) => Log -> m ()
journal_ = journal . fromList . discardNewlines

discardNewlines :: Log -> [Log]
discardNewlines (Log lt lc msg) = map (Log lt lc) (lines msg)

sinkLogs :: (MonadLogger m,MonadIO m) => m ()
sinkLogs = sink (traverse_ print)

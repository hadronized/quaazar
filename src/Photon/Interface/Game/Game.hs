-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Photon.Interface.Game where


-- TODO: replace all resulting a with Game a
runGame :: IO [Event] -> EventHandler a -> (a -> a) -> a -> IO ()
runGame pollEvents handler logic = run
  where
    run app = pollEvents >>= forwardEvents app >>= maybe (return ()) (run . logic)
    forwardEvents app events = return $ case events of
      [] -> Just app
      (x:xs) -> handler (x :| xs) app

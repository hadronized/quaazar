module Photon.Control.Monad (
    -- * Filter
    partitionM
  ) where

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p l = sel l ([],[])
  where
    sel [] a = return a
    sel (x:xs) (ts,fs) = do
        r <- p x
        sel xs $ if r then (x:ts,fs) else (ts,x:fs)

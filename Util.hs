{-# OPTIONS -W #-}

module Util (printListToFile, concurrently) where

import Control.Exception (finally)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import System.IO (withFile, IOMode(WriteMode), hPrint)

printListToFile :: (Show a) => FilePath -> [a] -> IO ()
printListToFile file xs = withFile file WriteMode
                            (\h -> mapM_ (hPrint h) xs)

concurrently :: [IO ()] -> IO ()
concurrently tasks = do
  mvars <- mapM (\task -> do mvar <- newEmptyMVar
                             forkIO (task `finally` putMVar mvar ())
                             return mvar)
                tasks
  mapM_ takeMVar mvars


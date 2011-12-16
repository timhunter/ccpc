{-# OPTIONS -W #-}

module Util (
    printListToFile, readListFromFileLines,
    encodeListToFile, decodeListFromFile,
    concurrently,
    StdRandom, random, randomR, runStdRandom, choose
) where

import Data.Binary (Binary(get, put))
import Data.Binary.Get (runGet, isEmpty)
import Data.Binary.Put
import Control.Exception (bracket, finally)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import System.IO (withFile, IOMode(ReadMode, WriteMode), hPrint, hGetLine, hFlush, hIsEOF, hClose, openBinaryFile)
import qualified Control.Monad.State.Lazy as State
import qualified Data.ByteString.Lazy as B
import qualified System.Random as R

printListToFile :: (Show a) => FilePath -> [a] -> IO ()
printListToFile file xs = withFile file WriteMode
                            (\h -> mapM_ (\x -> hPrint h x >> hFlush h) xs)

readListFromFileLines :: (Read a) => FilePath -> IO [a]
readListFromFileLines file = withFile file ReadMode loop
  where loop h = do b <- hIsEOF h
                    if b then return [] else do l <- hGetLine h
                                                xs <- loop h
                                                return (read l : xs)

encodeListToFile :: (Binary a) => FilePath -> [a] -> IO ()
encodeListToFile file xs = bracket (openBinaryFile file WriteMode) hClose
  (\h -> mapM_ (\x -> B.hPut h (runPut (put x >> flush)) >> hFlush h) xs)

decodeListFromFile :: (Binary a) => FilePath -> IO [a]
decodeListFromFile file = do fmap (runGet loop) (B.readFile file)
  where loop = do b <- isEmpty
                  if b then return [] else do x <- get
                                              xs <- loop
                                              return (x:xs)

concurrently :: [IO ()] -> IO ()
concurrently tasks = do
  mvars <- mapM (\task -> do mvar <- newEmptyMVar
                             forkIO (task `finally` putMVar mvar ())
                             return mvar)
                tasks
  mapM_ takeMVar mvars

type StdRandom = State.State R.StdGen

random :: (R.RandomGen g, State.MonadState g m, R.Random a) => m a
random = do g <- State.get
            let (a,g') = R.random g
            State.put g'
            return a

randomR :: (R.RandomGen g, State.MonadState g m, R.Random a) => (a,a) -> m a
randomR b = do g <- State.get
               let (a,g') = R.randomR b g
               State.put g'
               return a

runStdRandom :: State.State R.StdGen a -> IO a
runStdRandom = R.getStdRandom . State.runState

choose :: [(Double, a)] -> Double -> a
choose []              r = error ("choose [] " ++ show r)
choose ((wt,rhs):rhss) r = let r' = r - wt in
                           if r' < 0 || null rhss then rhs else choose rhss r'

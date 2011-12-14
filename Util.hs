{-# OPTIONS -W #-}

module Util (
    printListToFile, readListFromFileLines,
    encodeListToFile, decodeListFromFile,
    concurrently
) where

import Data.Binary (Binary(get, put))
import Data.Binary.Get (runGet, isEmpty)
import Data.Binary.Put
import Control.Exception (bracket, finally)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import System.IO (withFile, IOMode(ReadMode, WriteMode), hPrint, hGetLine, hFlush, hIsEOF, hClose, openBinaryFile)
import qualified Data.ByteString.Lazy as B

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


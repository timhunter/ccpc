{-# OPTIONS -W #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util (
    putStrsLnToFile, getLinesFromFile,
    printListToFile, readListFromFileLines,
    encodeListToFile, decodeListFromFile,
    concurrently,
    StdRandom, random, randomR, runStdRandom, choose,
    MonoidMap(MonoidMap),
    isLeft, isRight, fromLeft, fromRight,
    buffered
) where

import Data.Binary (Binary(get, put))
import Data.Binary.Get (runGet, isEmpty)
import Data.Binary.Put
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid(..))
import Control.Exception (bracket, finally)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (NFData)
import Control.Parallel.Strategies (withStrategy, parBuffer, rdeepseq)
import System.IO (withFile, IOMode(ReadMode, WriteMode), hPutStrLn, hGetLine, hFlush, hIsEOF, hClose, openBinaryFile)
import qualified Control.Monad.State.Lazy as State
import qualified Data.ByteString.Lazy as B
import qualified System.Random as R
import qualified Data.Map as M

putStrsLnToFile :: FilePath -> [String] -> IO ()
putStrsLnToFile file xs = withFile file WriteMode
                            (\h -> mapM_ (\x -> hPutStrLn h x >> hFlush h) xs)

printListToFile :: (Show a) => FilePath -> [a] -> IO ()
printListToFile file xs = putStrsLnToFile file (map show xs)

getLinesFromFile :: FilePath -> IO [String]
getLinesFromFile file = withFile file ReadMode loop
  where loop h = do b <- hIsEOF h
                    if b then return [] else do l <- hGetLine h
                                                xs <- loop h
                                                return (l : xs)

readListFromFileLines :: (Read a) => FilePath -> IO [a]
readListFromFileLines = fmap (map read) . getLinesFromFile

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

newtype MonoidMap k v = MonoidMap (M.Map k v)
  deriving (Functor, Foldable, Traversable, Eq, Ord, Read, Show)
instance (Ord k, Monoid v) => Monoid (MonoidMap k v) where
  mempty = MonoidMap M.empty
  mappend (MonoidMap m1) (MonoidMap m2) = MonoidMap (M.unionWith mappend m1 m2)
  mconcat ms = MonoidMap (M.unionsWith mappend [ m | MonoidMap m <- ms ])

isLeft, isRight :: Either a b -> Bool
isLeft  (Left  _) = True
isLeft  _         = False
isRight (Right _) = True
isRight _         = False

fromLeft  :: Either a b -> a
fromLeft  (Left  x) = x
fromLeft  _         = error "Util.fromLeft: Right"
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _         = error "Util.fromRight: Left"

buffered :: (NFData a) => [a] -> [a]
buffered = withStrategy (parBuffer 3 rdeepseq)

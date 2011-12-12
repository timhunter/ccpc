{-# OPTIONS -W #-}

module Generate (main) where

import Data.List (intercalate)
import Control.Monad (liftM, forever)
import Control.Monad.State.Lazy (MonadState, get, put, runState)
import CFG (CFG, Vertex)
import Partition (partition)
import Util (printListToFile)
import Data.Binary (decodeFile)
import Control.Concurrent (forkIO)
import qualified System.Random as R
import qualified Data.Array as A
import qualified Data.ByteString.Lazy.Char8 as B

data Item = LB | RB | Fail | Lex Vertex

main :: IO ()
main = do
  theCFG <- decodeFile "wsj.cfg"
  let (theSCCs, thePartitionList, thePartition) = partition 1e-20 1000 theCFG
  printListToFile "wsj.sccs" theSCCs
  printListToFile "wsj.partitionlist" thePartitionList
  printListToFile "wsj.partition" (A.elems thePartition)
  theWordsFile <- B.readFile "wsj.words"
  let theWords = let bs = B.lines theWordsFile
                 in A.listArray (1, length bs) bs
      show' LB      = "["
      show' RB      = "]"
      show' Fail    = "?"
      show' (Lex n) = B.unpack (theWords A.! (-n))
  forever $ R.getStdRandom (runState (generate theCFG thePartition 0))
            >>= putStrLn . intercalate " " . map show'

random :: (R.RandomGen g, MonadState g m, R.Random a) => m a
random = get >>= \g -> let (a,g') = R.random g in put g' >> return a

randomR :: (R.RandomGen g, MonadState g m, R.Random a) => (a,a) -> m a
randomR b = get >>= \g -> let (a,g') = R.randomR b g in put g' >> return a

generate :: (R.RandomGen g, MonadState g m) =>
            CFG -> A.Array Vertex Double -> Vertex -> m [Item]
generate g p = f 20 where
  f fuel lhs | fuel < 0  = return [Fail]
             | lhs < 0   = return [Lex lhs]
             | otherwise = randomR (0, p A.! lhs)
	                   >>= liftM concat'
                             . mapM (f (pred fuel))
                             . choose [ (wt * product (map (p A.!) rhs), rhs)
			              | (wt, rhs) <- g A.! lhs ]
  concat' xs | length xs > 2 = [LB] ++ concat xs ++ [RB]
             | otherwise     =         concat xs

choose :: [(Double, a)] -> Double -> a
choose []              r = error ("choose [] " ++ show r)
choose ((wt,rhs):rhss) r = let r' = r - wt in
                           if r' < 0 || null rhss then rhs else choose rhss r'

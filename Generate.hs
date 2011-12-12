{-# OPTIONS -W #-}

module Generate (main) where

import Data.List (intercalate)
import Control.Monad (liftM, forever)
import Control.Monad.State.Lazy (MonadState, get, put, runState)
import CFG (CFG, Vertex, isTerminal)
import Data.Binary (decodeFile)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import qualified System.Random as R
import qualified Data.ByteString.Lazy.Char8 as B

data Item = LB | RB | Fail | Lex Vertex

main :: IO ()
main = do
  theCFG <- decodeFile "wsj.cfg"
  thePartitionFile <- B.readFile "wsj.partition"
  theWordsFile <- B.readFile "wsj.words"
  let thePartition :: UArray Vertex Double
      thePartition = listArray (bounds theCFG)
                       (map (read . B.unpack) (B.lines thePartitionFile))
      theWords :: Array Vertex B.ByteString
      theWords = let bs = B.lines theWordsFile
                 in listArray (1, length bs) bs
      show' :: Item -> String
      show' LB      = "["
      show' RB      = "]"
      show' Fail    = "?"
      show' (Lex n) = B.unpack (theWords ! (-n))
  forever $ R.getStdRandom (runState (generate theCFG thePartition 0))
            >>= putStrLn . intercalate " " . map show'

random :: (R.RandomGen g, MonadState g m, R.Random a) => m a
random = get >>= \g -> let (a,g') = R.random g in put g' >> return a

randomR :: (R.RandomGen g, MonadState g m, R.Random a) => (a,a) -> m a
randomR b = get >>= \g -> let (a,g') = R.randomR b g in put g' >> return a

generate :: (R.RandomGen g, MonadState g m) =>
            CFG -> UArray Vertex Double -> Vertex -> m [Item]
generate g p = f 20 where
  f fuel lhs
    | fuel < 0       = return [Fail]
    | isTerminal lhs = return [Lex lhs]
    | otherwise      = randomR (0, p ! lhs)
                       >>= liftM concat'
                         . mapM (f (pred fuel))
                         . choose [ (wt * product (map part rhs), rhs)
                                  | (wt, rhs) <- g ! lhs ]
  part cat | isTerminal cat = 1
           | otherwise      = p ! cat
  concat' xs | length xs > 2 = [LB] ++ concat xs ++ [RB]
             | otherwise     =         concat xs

choose :: [(Double, a)] -> Double -> a
choose []              r = error ("choose [] " ++ show r)
choose ((wt,rhs):rhss) r = let r' = r - wt in
                           if r' < 0 || null rhss then rhs else choose rhss r'

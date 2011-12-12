{-# OPTIONS -W #-}

module Generate (main) where

import Data.List (intercalate)
import Control.Monad (liftM, forever)
import Control.Monad.State.Lazy (MonadState, get, put, runState)
import CFG (CFG, Vertex)
import Data.Binary (decodeFile)
import qualified System.Random as R
import qualified Data.Array as A
import qualified Data.ByteString.Lazy.Char8 as B

data Item = LB | RB | Fail | Lex Vertex

main :: IO ()
main = do
  theCFG <- decodeFile "wsj.cfg"
  theWordsFile <- B.readFile "wsj.words"
  let theWords = let bs = B.lines theWordsFile
                 in A.listArray (1, length bs) bs
      show' LB      = "["
      show' RB      = "]"
      show' Fail    = "?"
      show' (Lex n) = B.unpack (theWords A.! (-n))
  forever $ R.getStdRandom (runState (generate theCFG 0))
            >>= putStrLn . intercalate " " . map show'

random :: (R.RandomGen g, MonadState g m, R.Random a) => m a
random = get >>= \g -> let (a,g') = R.random g in put g' >> return a

generate :: (R.RandomGen g, MonadState g m) => CFG -> Vertex -> m [Item]
generate g = f 20 where
  f fuel lhs | fuel < 0  = return [Fail]
             | lhs < 0   = return [Lex lhs]
             | otherwise = random >>= liftM concat'
                                    . mapM (f (pred fuel))
                                    . choose (g A.! lhs)
  concat' xs | length xs > 2 = [LB] ++ concat xs ++ [RB]
             | otherwise     =         concat xs

choose :: [(Double, a)] -> Double -> a
choose ((wt,rhs):rhss) r = let r' = r - wt in
                           if r' < 0 then rhs else choose rhss r'

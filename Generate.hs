{-# OPTIONS -W #-}

module Generate (main, top, generate) where

import Control.Monad (liftM, liftM2, forever, foldM)
import CFG (CFG, Vertex, isTerminal)
import Chart (Chart(..), Cell(Universe), infixChart)
import Data.Binary (decodeFile)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Util (decodeListFromFile, StdRandom, runStdRandom, randomR, choose)
import Control.Parallel (par)
import Control.Parallel.Strategies (withStrategy, parList, rseq)
import Merge (toListBy, singleton, crossWithAscList)
import qualified Data.ByteString.Lazy.Char8 as B

data Item = LB | RB | Fail | Lex Vertex deriving (Show)
type Phrase = [Item]
type Stream = [(Double, Maybe Phrase)]

showPhraseBy :: (a -> String) -> Array Vertex a -> Phrase -> String
showPhraseBy unpack words = go where
  go  []          = ""
  go  (LB   : xs) = "["                   ++ go  xs
  go  (RB   : xs) = "]"                   ++ go' xs
  go  (Fail : xs) = "?"                   ++ go' xs
  go  (Lex n: xs) = unpack (words ! (-n)) ++ go' xs
  go' []          = ""
  go' (RB   : xs) = "]" ++ go' xs
  go' xs          = " " ++ go  xs

showStreamBy :: (a -> String) -> Array Vertex a -> Stream -> IO ()
showStreamBy unpack words stream = foldM go 0 stream >> return () where
  go n (wt, Nothing) = if n > 0 then return (n - 1) else
                       putStr (show wt ++ "\ESC[K\r") >> return 10000
  go _ (wt, Just ph) = putStrLn (show wt ++ " " ++ showPhrase ph) >> return 0
  showPhrase = showPhraseBy unpack words

main :: IO ()
main = do
  theWordsFile <- B.readFile "wsj.words" :: IO B.ByteString
  let theWords :: Array Vertex B.ByteString
      theWords = let bs = B.lines theWordsFile
                 in listArray (1, length bs) bs
  theCFG <- theWords `par` decodeFile "wsj.cfg" :: IO CFG
  let theChart :: Chart
      theChart = infixChart [-4070 {-car/NN-}, -9756 {-dealer/NN-}]
      thePinnacle :: Cell
      thePinnacle = last (last (levels theChart))
  known <- fmap (listArray (0, indexOfCell theChart thePinnacle)
                 . withStrategy (parList rseq))
                (decodeListFromFile "wsj.car-dealer.partition")
           :: IO (Array Int (UArray Vertex Double))
  -- forever $ runStdRandom (generate theCFG theChart known thePinnacle 0)
  --           >>= putStrLn . showPhraseBy B.unpack theWords
  showStreamBy B.unpack theWords
    (top theCFG (known ! indexOfCell theChart Universe) ! 0)

-- Try: let prt = head (Partition.partitions CFG.aCFG (Chart.infixChart [-1]) 1000 (Graph.sccL (CFG.graphOfCFG CFG.aCFG))) in mapM_ print (take 10 (top CFG.aCFG prt ! 0))
-- Try: let (words, cfg) = CFG.cfgOfMCFG (MCFGRead.mcfgFromFile "grammars/wmcfg/strauss.wmcfg"); prt = Partition.partitions cfg (infixChart []) 1000 (Graph.sccL (CFG.graphOfCFG cfg)) in showStreamBy id words (top cfg (head prt) ! 0)

descend :: [(Double, Maybe a)] -> [(Double, Maybe a)]
descend [] = []
descend ((wt, x) : xs) = (wt, x) : go wt xs where
  go _   [] = []
  go wt0 ((wt, Nothing) : xs) | wt >= wt0 = go wt xs
  go _   ((wt, x      ) : xs) = (wt, x) : go wt xs

top :: CFG -> UArray Vertex Double -> Array Vertex Stream
top cfg prt = a where
  a = listArray (bounds cfg) (map f (indices cfg))
  f lhs = map (fmap (fmap concat'))
        $ descend
        $ (prt ! lhs, Nothing) -- prt bounds the tropical partition function from above
        : toListBy cmp ((cfg ! lhs) >>= \(wt, rhs) ->
          foldr prepend (singleton (wt, Just [])) rhs)
  prepend v | isTerminal v = map (fmap (fmap (fmap ([Lex v] :))))
            | otherwise    = crossWithAscList multiply (a ! v)
  multiply (p,xs) (q,ys) = (p*q, liftM2 (:) xs ys)
  cmp (p,_) (q,_)        = compare q p

generate :: CFG -> Chart -> Array Int (UArray Vertex Double) ->
            Cell -> Vertex -> StdRandom Phrase
generate cfg Chart{epsilon=epsilon,
                   terminal=terminal,
                   splits=splits,
                   indexOfCell=indexOfCell} known = f 100 where
  f fuel here lhs
    | fuel < 0       = return [Fail]
    | isTerminal lhs = return [Lex lhs]
    | otherwise      = randomR (0, known ! indexOfCell here ! lhs)
                       >>= liftM concat'
                         . mapM (uncurry (f (pred fuel)))
                         . choose [ (wt * wt', yield')
                                  | (wt, rhs) <- cfg ! lhs
                                  , (wt', yield') <- interp' here rhs ]
  interp' :: Cell -> [Vertex] -> [(Double, [(Cell, Vertex)])]
  interp' cell [] = [(epsilon cell, [])]
  interp' cell [v] = [(if isTerminal v
                         then terminal cell v
                         else known ! indexOfCell cell ! v,
                       [(cell, v)])]
  interp' cell (v:vs) = [ (wt'l * wt'r, yield'l ++ yield'r)
                        | (l, r) <- splits cell
                        , let interp'l = interp' l [v]
                              interp'r = interp' r vs
                        , not (null interp'r)
                        , (wt'l, yield'l) <- interp'l
                        , (wt'r, yield'r) <- interp'r ]

concat' :: [[Item]] -> [Item]
concat' xs | length xs > 1 = [LB] ++ concat xs ++ [RB]
           | otherwise     =         concat xs

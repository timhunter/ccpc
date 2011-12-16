{-# OPTIONS -W #-}

module Generate (main, top, generate) where

import Data.List (intercalate)
import Control.Monad (liftM, liftM2, forever)
import CFG (CFG, Vertex, isTerminal)
import Chart (Chart(..), Cell(Universe), infixChart)
import Data.Binary (decodeFile)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Util (decodeListFromFile, StdRandom, runStdRandom, randomR, choose)
import Control.Parallel (par)
import Control.Parallel.Strategies (withStrategy, parList, rseq)
import Merge (mergeBy, crossByWith)
import qualified Data.ByteString.Lazy.Char8 as B

data Item = LB | RB | Fail | Lex Vertex deriving (Show)
type Phrase = [Item]
type Stream = [(Double, Maybe Phrase)]

main :: IO ()
main = do
  theWordsFile <- B.readFile "wsj.words" :: IO B.ByteString
  let theWords :: Array Vertex B.ByteString
      theWords = let bs = B.lines theWordsFile
                 in listArray (1, length bs) bs
      show' :: Item -> String
      show' LB      = "["
      show' RB      = "]"
      show' Fail    = "?"
      show' (Lex n) = B.unpack (theWords ! (-n))
      showPhrase :: Phrase -> String
      showPhrase = intercalate " " . map show'
  theCFG <- theWords `par` decodeFile "wsj.cfg" :: IO CFG
  let theChart :: Chart
      theChart = infixChart [-4070 {-car/NN-}, -9756 {-dealer/NN-}]
      thePinnacle :: Cell
      thePinnacle = last (last (levels theChart))
  known <- fmap (listArray (0, indexOfCell theChart thePinnacle)
                 . withStrategy (parList rseq))
                (decodeListFromFile "wsj.car-dealer.partition"
                 :: IO [UArray Vertex Double])
  forever $ runStdRandom (generate theCFG theChart known thePinnacle 0)
            >>= putStrLn . showPhrase
  mapM_ (\(wt, phrase) -> do
          putStr (show wt)
          putStrLn (case phrase of Nothing -> ""
                                   Just phrase -> " " ++ showPhrase phrase))
        (top theCFG (known ! indexOfCell theChart Universe) 0)

-- Try: let prt = head (partitions aCFG (infixChart [-1]) 1000 (sccL (graphOfCFG aCFG))) in take 10 (top aCFG prt 0)

top :: CFG -> UArray Vertex Double -> Vertex -> Stream
top cfg prt = f where
  f lhs
    | isTerminal lhs = [(1, Just [Lex lhs])]
    | otherwise      = (prt ! lhs, Nothing) -- prt overapproximates
                                            -- the tropical partition function
                       : mergeBy cmp
                           [ map (scale wt)
                                 (case rhs of [] -> identity
                                              _  -> foldr1 cross (map f rhs))
                           | (wt, rhs) <- cfg ! lhs ]
  cross                  = crossByWith cmp multiply
  scale p (q,ys)         = (p*q, ys)
  multiply (p,xs) (q,ys) = (p*q, liftM2 (++) xs ys)
  identity               = [(1, Just [])]
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
  concat' xs | length xs > 2 = [LB] ++ concat xs ++ [RB]
             | otherwise     =         concat xs

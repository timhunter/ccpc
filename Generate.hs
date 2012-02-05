{-# OPTIONS -W -fspec-constr-count=10 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Generate (main, top, generate) where

import CFG
import MCFGRead (mcfgFromFile)
import MCFG (cfgOfMCFG, portrayMCFG)
import Graph (SCCL, sccL)
import Chart (Chart(..), Cell, infixChart, prefixChart, suffixChart, exactChart)
import Partition (partitions)
import Util (decodeListFromFile, StdRandom, runStdRandom, randomR, choose)
import Merge (toListBy, empty, singleton, fromAscList, crossWithAscList)
import Control.Monad (liftM2, forever, foldM)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (using, parList, evalList, rseq)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Binary (decodeFile)
import Data.List (isSuffixOf)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M

data Words = Words {
  indexOfWord :: B.ByteString -> Int {-positive-},
  wordOfIndex :: Int {-positive-} -> B.ByteString
}

data Method = Random | Top

data Options = Options {
  theCFG       :: CFGYield,
  theWords     :: Words,
  theSCCs      :: [SCCL],
  theChart     :: Chart,
  thePartition :: [UArray Vertex Double],
  method       :: Method
}

optionsRepartition :: Options -> Options
optionsRepartition o@Options{theCFG   = CFGYield theCFG _,
                             theSCCs  = theSCCs,
                             theChart = theChart}
  = o{thePartition = partitions theCFG theChart 1000 theSCCs}

optionsSetCFG :: CFG Double yield -> Portray yield -> Options -> Options
optionsSetCFG cfg portray o
  = optionsRepartition
  $ o{theCFG  = CFGYield cfg portray,
      theSCCs = sccL (graphOfCFG cfg) {- +RTS -K1g -}}

optionsSetWords :: Array Vertex B.ByteString -> Options -> Options
optionsSetWords words
  = \o -> o{theWords = Words{indexOfWord = indexOfWord,
                             wordOfIndex = (words !)}} where
  swapped = M.fromList (map swap (assocs words))
  indexOfWord w = case M.lookup w swapped of
                  Nothing -> error ("Unknown word " ++ B.unpack w)
                  Just i  -> i

optionsDefault :: Options
optionsDefault
  = optionsSetCFG aCFG portrayCFG
  $ Options{theCFG       = undefined,
            theWords     = Words (fst . fromJust . B.readInt) (B.pack . show),
            theSCCs      = undefined,
            theChart     = infixChart [],
            thePartition = undefined,
            method       = Random}

getOptChart :: ([Vertex] -> Chart) -> [String] -> Options -> IO Options
getOptChart f (sentence:xs) o@Options{theWords=Words{indexOfWord=indexOfWord}}
  = let terminals = map (negate.indexOfWord) (B.words (B.pack sentence))
                    `using` evalList rseq
        chart = f terminals
    in terminals `seq` chart `seq` -- detect problems with the sentence early
       getOpt xs (optionsRepartition o{theChart = chart})
getOptChart _ [] _
  = error "Sentence missing at the end of the command line"

getOpt :: [String] -> Options -> IO Options
getOpt [] o = return o
getOpt (x:xs) o
  | ".wmcfg" `isSuffixOf` x
  = case cfgOfMCFG (mcfgFromFile x) of
    (words, cfg) -> getOpt xs (optionsSetCFG cfg portrayMCFG
                                (optionsSetWords (fmap B.pack words) o))
  | ".cfg" `isSuffixOf` x -- created by Grammar.main
  = decodeFile x >>= \cfg -> getOpt xs (optionsSetCFG cfg portrayCFG o)
  | ".words" `isSuffixOf` x -- created by Grammar.main
  = fmap B.lines (B.readFile x) >>= \bs ->
    getOpt xs (optionsSetWords (listArray (1, length bs) bs) o)
  | ".sccs" `isSuffixOf` x -- created by Partition.main
  = decodeListFromFile x >>= \sccs ->
    getOpt xs (optionsRepartition o{theSCCs = sccs})
  | ".partition" `isSuffixOf` x -- created by Partition.main
  = decodeListFromFile x >>= \known ->
    getOpt xs o{thePartition = known `using` parList rseq}
getOpt ("-infix" :xs) o = getOptChart infixChart  xs o
getOpt ("-exact" :xs) o = getOptChart exactChart  xs o
getOpt ("-p"     :xs) o = getOptChart prefixChart xs o
getOpt ("-prefix":xs) o = getOptChart prefixChart xs o
getOpt ("-suffix":xs) o = getOptChart suffixChart xs o
getOpt ("-random":xs) o = getOpt xs o{method=Random}
getOpt ("-top"   :xs) o = getOpt xs o{method=Top   }
getOpt (x:_)          _ = error ("Unknown option " ++ x)

main :: IO ()
main = do
  o <- getArgs >>= \args -> getOpt args optionsDefault
  case theCFG o of { CFGYield cfg portrayBy ->
  let Chart{minCell    =minCell,
            maxCell    =maxCell,
            indexOfCell=indexOfCell} = theChart o
      known = theWords o `par` cfg `pseq`
              listArray (indexOfCell minCell, indexOfCell maxCell)
                        (thePartition o)
      portray = portrayBy (B.unpack . wordOfIndex (theWords o)) in
  case method o of
    Random -> forever $ do
      ph <- runStdRandom (generate cfg (theChart o) known maxCell 0)
      putStrLn (portray ph)
      hFlush stdout
    Top -> showStreamBy portray (top cfg (theChart o) known maxCell 0) }

{- Usage examples:
    ./Generate -top
    ./Generate -top wsj.cfg wsj.words -infix "car/NN dealer/NN" wsj.car-dealer.partition
    ./Generate -top grammars/wmcfg/strauss.wmcfg -exact "the dog hit the dog with the stick with Jon with Jon with Jon with Jon with Jon"
-}

type Stream prob yield = [(prob, Maybe (Derivation yield))]

showStreamBy :: (Show prob) => (Derivation yield -> String) ->
                Stream prob yield -> IO ()
showStreamBy portray stream = foldM go 0 stream >> return () where
  go n (wt, Nothing)
    | n > 0 = return (n - 1)
    | otherwise = do
      putStr (show wt ++ "\ESC[K\r")
      hFlush stdout
      return 10000
  go _ (wt, Just ph) = do
      putStrLn (show wt ++ " " ++ portray ph)
      hFlush stdout
      return 0

-- Shorten a stream of descending-probability maybe-results by removing
-- duplicate probabilities and stopping at zero probability.
descend :: (Ord prob, Num prob) => [(prob, Maybe a)] -> [(prob, Maybe a)]
descend []             = []
descend ((0 , _) : _ ) = []
descend ((wt, x) : xs) = (wt, x) : go wt xs where
  go _   []                   = []
  go _   ((0 , _      ) : _ ) = []
  go wt0 ((wt, Nothing) : xs) | wt >= wt0 = go wt xs
  go _   ((wt, x      ) : xs) = (wt, x) : go wt xs

-- Make an array from its bounds and a function from indices to elements.
funArray :: (IArray a e, Ix i) => (i, i) -> (i -> e) -> a i e
funArray bounds f = listArray bounds (map f (range bounds))

-- Generate parses in descending probability order.
top :: forall yield.
       CFG Double yield -> Chart -> Array Int (UArray Vertex Double) ->
       Cell -> Vertex -> Stream Double yield
top cfg Chart{minCell=minCell,
              maxCell=maxCell,
              indexOfCell=indexOfCell,
              cellOfIndex=cellOfIndex,
              epsilon=epsilon,
              terminal=terminal,
              splits=splits} known = f where
  f :: Cell -> Vertex -> Stream Double yield
  f here lhs = a ! indexOfCell here ! lhs
  a :: Array Int (Array Vertex (Stream Double yield))
  a = funArray (indexOfCell minCell, indexOfCell maxCell) $ \hereIndex ->
      let here = cellOfIndex hereIndex in
      funArray (bounds cfg) $ \lhs ->
      descend $
      (known {- the partition function bounds the tropical partition function from above -} ! hereIndex ! lhs, Nothing) :
      toListBy cmp
      [ (fmap . fmap . fmap) (Node (Step lhs y)) tree
      | RHS{prob=p, children=cs, yield=y} <- cfg ! lhs
      , tree <- interp' here p cs ]
  interp' :: Cell -> Double -> [Vertex] ->
             Forest (Double, Maybe [Derivation yield])
  interp' cell wt [] = con wt (epsilon cell) []
  interp' cell wt [v]
    | isTerminal v = con wt (terminal cell v) [lex v]
    | otherwise = fromAscList [ (wt * wt', fmap (:[]) yield')
                              | (wt', yield') <- a ! indexOfCell cell ! v ]
  interp' cell wt (v:vs) = do
    (l, r) <- splits cell
    let rForest = interp' r wt vs
    if isTerminal v
      then let wt' = terminal l v in
           if wt' > 0 then map (fmap (multiply (wt', Just (lex v)))) rForest
                      else empty
      else crossWithAscList multiply (a ! indexOfCell l ! v) rForest
  con :: Double -> Double -> [Derivation yield] ->
         Forest (Double, Maybe [Derivation yield])
  con wt wt' ph = if wt' > 0 then singleton (wt * wt', Just ph) else empty
  multiply :: (Double, Maybe (Derivation yield)) ->
              (Double, Maybe [Derivation yield]) ->
              (Double, Maybe [Derivation yield])
  multiply (p,xs) (q,ys) = (p*q, liftM2 (:) xs ys)
  cmp :: (Double, a) -> (Double, a) -> Ordering
  cmp (p,_) (q,_)        = compare q p
  lex :: Vertex -> Derivation yield
  lex v = Node (Step v (error "lex")) []

-- Generate parses randomly.
generate :: forall yield.
            CFG Double yield -> Chart -> Array Int (UArray Vertex Double) ->
            Cell -> Vertex -> StdRandom (Derivation yield)
generate cfg Chart{epsilon=epsilon,
                   terminal=terminal,
                   splits=splits,
                   indexOfCell=indexOfCell} known = f 100 where
  f fuel here lhs
    | fuel < 0       = return (Node (Step lhs (error "out of fuel")) [])
    | isTerminal lhs = return (Node (Step lhs (error "is terminal")) [])
    | otherwise      = do
      r <- randomR (0, known ! indexOfCell here ! lhs)
      let (y, below) = choose [ (wt * wt', (y, below))
                              | RHS{prob=wt, children=rhs, yield=y} <- cfg ! lhs
                              , (wt', below) <- interp' here rhs ]
                              r
      below <- mapM (uncurry (f (pred fuel))) below
      return (Node (Step lhs y) below)
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

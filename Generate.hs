{-# OPTIONS -W -fspec-constr-count=10 #-}

module Generate (main, top, generate) where

import MCFGRead (mcfgFromFile)
import CFG (CFG, Vertex, isTerminal, aCFG, cfgOfMCFG, graphOfCFG)
import Graph (SCCL, sccL)
import Chart (Chart(..), Cell, infixChart, prefixChart, suffixChart, exactChart)
import Partition (partitions)
import Util (decodeListFromFile, StdRandom, runStdRandom, randomR, choose)
import Merge (Forest, toListBy, empty, singleton, fromAscList, crossWithAscList)
import Control.Monad (liftM, liftM2, forever, foldM)
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
  theCFG       :: CFG,
  theWords     :: Words,
  theSCCs      :: [SCCL],
  theChart     :: Chart,
  thePartition :: [UArray Vertex Double],
  method       :: Method
}

optionsRepartition :: Options -> Options
optionsRepartition o@Options{theCFG=theCFG,theSCCs=theSCCs,theChart=theChart}
  = o{thePartition = partitions theCFG theChart 1000 theSCCs}

optionsSetCFG :: CFG -> Options -> Options
optionsSetCFG cfg o
  = optionsRepartition
  $ o{theCFG = cfg, theSCCs = sccL (graphOfCFG cfg) {- +RTS -K1g -}}

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
  = optionsSetCFG aCFG
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
  = case cfgOfMCFG ((map.fmap) B.pack (mcfgFromFile x)) of
    (words, cfg) -> getOpt xs (optionsSetCFG cfg (optionsSetWords words o))
  | ".cfg" `isSuffixOf` x -- created by Grammar.main
  = decodeFile x >>= \cfg -> getOpt xs (optionsSetCFG cfg o)
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
  let Chart{minCell    =minCell,
            maxCell    =maxCell,
            indexOfCell=indexOfCell} = theChart o
      known = theWords o `par` theCFG o `pseq`
              listArray (indexOfCell minCell, indexOfCell maxCell)
                        (thePartition o)
      w = wordOfIndex (theWords o)
  case method o of
    Random -> forever $ do
      ph <- runStdRandom (generate (theCFG o) (theChart o) known maxCell 0)
      putStrLn (showPhraseBy w ph)
      hFlush stdout
    Top -> showStreamBy w (top (theCFG o) (theChart o) known maxCell 0)

{- Usage examples:
    ./Generate -top
    ./Generate -top wsj.cfg wsj.words wsj.car-dealer.partition -infix "car/NN dealer/NN"
    ./Generate -top grammars/wmcfg/strauss.wmcfg -exact "the dog hit the dog with the stick with Jon with Jon with Jon with Jon with Jon"
-}

data Item = LB | RB | Fail | Lex Vertex deriving (Show)
type Phrase = [Item]
type Stream = [(Double, Maybe Phrase)]

showPhraseBy :: (Int -> B.ByteString) -> Phrase -> String
showPhraseBy w = go where
  go  []          = ""
  go  (LB   : xs) = "["               ++ go  xs
  go  (RB   : xs) = "]"               ++ go' xs
  go  (Fail : xs) = "?"               ++ go' xs
  go  (Lex n: xs) = B.unpack (w (-n)) ++ go' xs
  go' []          = ""
  go' (RB   : xs) = "]" ++ go' xs
  go' xs          = " " ++ go  xs

showStreamBy :: (Int -> B.ByteString) -> Stream -> IO ()
showStreamBy w stream = foldM go 0 stream >> return () where
  go n (wt, Nothing)
    | n > 0 = return (n - 1)
    | otherwise = do
      putStr (show wt ++ "\ESC[K\r")
      hFlush stdout
      return 10000
  go _ (wt, Just ph) = do
      putStrLn (show wt ++ " " ++ showPhrase ph)
      hFlush stdout
      return 0
  showPhrase = showPhraseBy w

-- Shorten a stream of descending-probability maybe-results by removing
-- duplicate probabilities and stopping at zero probability.
descend :: [(Double, Maybe a)] -> [(Double, Maybe a)]
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
top :: CFG -> Chart -> Array Int (UArray Vertex Double) ->
       Cell -> Vertex -> Stream
top cfg Chart{minCell=minCell,
              maxCell=maxCell,
              indexOfCell=indexOfCell,
              cellOfIndex=cellOfIndex,
              epsilon=epsilon,
              terminal=terminal,
              splits=splits} known = f where
  f :: Cell -> Vertex -> Stream
  f here lhs = a ! indexOfCell here ! lhs
  a :: Array Int (Array Vertex Stream)
  a = funArray (indexOfCell minCell, indexOfCell maxCell) $ \hereIndex ->
      let here = cellOfIndex hereIndex in
      funArray (bounds cfg) $ \lhs ->
      map (fmap (fmap concat')) $
      descend $
      (known {- the partition function bounds the tropical partition function from above -} ! hereIndex ! lhs, Nothing) :
      toListBy cmp ((cfg ! lhs) >>= interp' here)
  interp' :: Cell -> (Double, [Vertex]) -> Forest (Double, Maybe [Phrase])
  interp' cell (wt, []) = con wt (epsilon cell) []
  interp' cell (wt, [v])
    | isTerminal v = con wt (terminal cell v) [[Lex v]]
    | otherwise = fromAscList [ (wt * wt', fmap (:[]) yield')
                              | (wt', yield') <- a ! indexOfCell cell ! v ]
  interp' cell (wt, v:vs) = do
    (l, r) <- splits cell
    let rForest = interp' r (wt, vs)
    if isTerminal v
      then let wt' = terminal l v in
           if wt' > 0 then map (fmap (multiply (wt', Just [Lex v]))) rForest
                      else empty
      else crossWithAscList multiply (a ! indexOfCell l ! v) rForest
  con :: Double -> Double -> [Phrase] -> Forest (Double, Maybe [Phrase])
  con wt wt' ph = if wt' > 0 then singleton (wt * wt', Just ph) else empty
  multiply :: (Double, Maybe Phrase) -> (Double, Maybe [Phrase])
                                     -> (Double, Maybe [Phrase])
  multiply (p,xs) (q,ys) = (p*q, liftM2 (:) xs ys)
  cmp :: (Double, a) -> (Double, a) -> Ordering
  cmp (p,_) (q,_)        = compare q p

-- Generate parses randomly.
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

-- Insert brackets to indicate binary (and ternary...) branching.
concat' :: [[Item]] -> [Item]
concat' xs | length xs > 1 = [LB] ++ concat xs ++ [RB]
           | otherwise     =         concat xs

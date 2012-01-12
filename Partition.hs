{-# OPTIONS -W #-}

module Partition (partition, partitions, iteration, main) where

import CFG (CFG, graphOfCFG, isTerminal)
import Chart (Chart(..), Cell, infixChart)
import Graph (Vertex, SCCL(..), sccL)
import Broyden (broyden, zero, Doubles)
import Data.Array.Unboxed (UArray)
import Data.Binary (decodeFile)
import Nomial (Nomial, con, var, subst)
import Data.Bifunctor (bimap)
import Data.Array.IArray
import Debug.Trace (trace)
import Control.Parallel (par)
import Control.Parallel.Strategies (using, parBuffer, rseq)
import Control.Exception (evaluate)
import Stage (showADouble, paginateCode, withFortran)
import Util (encodeListToFile, decodeListFromFile)
import qualified Data.IntMap as IM
import qualified Data.Traversable as T
import qualified Data.Vector.Storable as S
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

type Formula = Nomial Vertex Double

-- Try: let (words,cfg) = CFG.cfgOfMCFG (MCFGRead.mcfgFromFile "grammars/wmcfg/larsonian1.wmcfg") in (words, partitions cfg (Chart.infixChart []) 1000 (Graph.sccL (CFG.graphOfCFG cfg)))

main :: IO ()
main = do
  theCFG <- decodeFile "wsj.cfg"
  theSCCs <- theCFG `par`
             if True
               then decodeListFromFile "wsj.sccs" >>= evaluate
               else do let theSCCs = sccL (graphOfCFG theCFG) -- +RTS -K1g
                       encodeListToFile "wsj.sccs" theSCCs
                       return theSCCs
  evaluate theCFG
  let theChart :: Chart
      theChart = infixChart -- [-4070 {-car/NN-}, -9756 {-dealer/NN-}]
                            -- [-5175 {-drug/NN-}, -9756 {-dealer/NN-}]
                            [-4807 {-card/NN-}, -9756 {-dealer/NN-}]
      thePartition :: [UArray Vertex Double]
      thePartition = partitions theCFG theChart 1000 theSCCs
  encodeListToFile "wsj.card-dealer.partition" thePartition

partitions :: CFG -> Chart -> Int -> [SCCL] -> [UArray Vertex Double]
partitions cfg chart stepsMax cs = loop (array (0,-1) []) (levels chart) where
  loop :: Array Int (UArray Vertex Double) -> [[Cell]] -> [UArray Vertex Double]
  loop _known [] = []
  loop known (cells:levels) = new ++ loop known' levels
    where (knownMin, knownMax) = bounds known
          known' = listArray (knownMin, knownMax + length new)
                             (elems known ++ new)
          new :: [UArray Vertex Double]
          new = [ array (bounds cfg)
                        (partition cfg chart known cell stepsMax cs)
                | cell <- cells ]
                `using` parBuffer 3 rseq

-- Convert a CFG RHS into a Formula by partially evaluating the RHS with
-- respect to the given chart, the given cell in the chart, and the given
-- partition values already computed for other cells in the chart.
interp :: CFG -> Chart -> Array Int (UArray Vertex Double) ->
          Cell -> (Vertex -> Formula) -> Vertex -> Formula
interp cfg Chart{epsilon=epsilon,
                 terminal=terminal,
                 splits=splits,
                 indexOfCell=indexOfCell} known here var' i =
  sum [ con wt * interp' here rhs | (wt, rhs) <- cfg!i ] where
  interp' :: Cell -> [Vertex] -> Formula
  interp' cell [] = con (epsilon cell)
  interp' cell [v] | isTerminal v = con (terminal cell v)
                   | cell == here = var' v
                   | otherwise    = con (known ! indexOfCell cell ! v)
  interp' cell (v:vs) = sum [ interp' l [v] * interp' r vs
                            | (l, r) <- splits cell ]

-- The vector-to-vector function whose least root we want.  The arguments are:
-- the CFG, the chart, the known partition values in the finished chart cells,
-- the cell to work on in the chart, the known partition values in this cell,
-- the strongly connected component to work on, and the current estimates.  The
-- return value is a table of the current estimates along with the difference
-- vector between the new estimates and the current estimates.
iteration :: CFG -> Chart -> Array Int (UArray Vertex Double) -> Cell ->
             Array Vertex Double -> SCCL -> Doubles -> (Doubles, Doubles)
iteration cfg chart known here
          known_here SCCL{members=members,nonlinking=nonlinking} =
  let linking :: IM.IntMap (Bool, Formula)
      linking = IM.fromList
        [ (i, (link, interp cfg chart known here var' i))
        | (is, link) <- [(members, True), (nonlinking, False)]
        , i <- is ]
      var' i | IM.member i linking = var i
             | otherwise           = con (known_here ! i)
  in
  \olds ->
  let diff    :: [Double]
      current :: IM.IntMap Double
      (current, diff) = runWriter (evalStateT (T.mapM go linking) 0)

      go :: (Bool, Formula) -> StateT Int (Writer [Double]) Double
      go (True , fml) = return (recalc fml)
      go (False, fml) = do old <- gets (olds S.!)
                           modify succ
                           tell [recalc fml - old]
                           return old

      recalc :: Formula -> Double
      recalc = subst (current IM.!)
  in
  (S.fromList (map (current IM.!) members), S.fromList diff)

-- Code-generator version of "iteration" above.
iterationGen :: CFG -> Chart -> Array Int (UArray Vertex Double) -> Cell ->
                Array Vertex Double -> SCCL -> [String]
iterationGen cfg chart known here
             known_here SCCL{members=members,nonlinking=nonlinking} =
  let membersMap, nonlinkingMap :: IM.IntMap Int
      membersMap    = IM.fromList (zip members    [0..])
      nonlinkingMap = IM.fromList (zip nonlinking [0..])

      formulaGen :: Formula -> String
      formulaGen = show . bimap (\n -> Shown ("c[" ++ show n ++ "]"))
                                (\x -> Shown (showADouble x))

      genNonlinking :: Vertex -> String
      genNonlinking i = current_i ++
                       " = o[" ++ show (nonlinkingMap IM.! i) ++ "];"
        where current_i = "c[" ++ show (membersMap    IM.! i) ++ "]"

      genMember :: Vertex -> String
      genMember i = case IM.lookup i nonlinkingMap of
              Nothing -> current_i ++ " = " ++ fml ++ ";"
              Just n  -> "d[" ++ show n ++ "] = " ++ fml ++
                                            " - o[" ++ show n ++ "];"
        where fml = formulaGen (interp cfg chart known here var' i)
              current_i = "c[" ++ show (membersMap IM.! i) ++ "]"
              var' i = case IM.lookup i membersMap of
                       Nothing -> con (known_here ! i)
                       Just n  -> var n
  in
  paginateCode ("const double o[" ++ show (IM.size nonlinkingMap) ++
                   "], double c[" ++ show (IM.size membersMap) ++
                   "], double d[" ++ show (IM.size nonlinkingMap) ++ "]")
               ("o,c,d")
               (map genNonlinking nonlinking ++ map genMember members)

newtype Shown = Shown String
instance Show Shown where show (Shown x) = x

-- Main function for computing the partition function.  The arguments are:
-- the CFG, the chart, the known partition values in the finished chart cells,
-- the cell to work on in the chart,
-- the
-- maximum number of iterations for each strongly connected component,
-- and the list of strongly connected components as computed by
-- Graph.sccL.CFG.graphOfCFG.  The return values are the nonterminals and their
-- Z values, in the form of a list (sorted in computation order).
partition :: CFG -> Chart -> Array Int (UArray Vertex Double) -> Cell ->
             Int -> [SCCL] -> [(Vertex, Double)]
partition cfg chart known here stepsMax cs = list where
  known_here = array (bounds cfg) list
  list = cs >>= \c@SCCL{members=members,nonlinking=nonlinking} ->
    let lm = length members
        ln = length nonlinking
        msg = "(" ++ show here           ++ ",SCCL{members={-size "
                  ++ show lm             ++ "-}"
                  ++ show (head members) ++ ":_,nonlinking={-size "
                  ++ show ln             ++ "-}_})"
        trace' = traceIf (ln >= 10 || lm >= 100)
        withF = if ln >= 5 || lm >= 10
                then let codes = iterationGen cfg chart known here known_here c
                     in withFortran lm ln codes
                else let f = iteration cfg chart known here known_here c
                     in ($ f)
        final = trace' msg $ withF (\f ->
          let (steps, solution)
                | ln == 0   = (0, S.empty)
                | otherwise = broyden (snd . f) stepsMax (zero ln)
          in trace' (msg ++ " took " ++ show steps ++ " steps") $
             traceIf (steps >= stepsMax) (msg ++ " didn't converge") $
             fst (f solution))
    in zipWith (\i n -> (i, final S.! n)) members [0..]
  traceIf True  = trace
  traceIf False = const id

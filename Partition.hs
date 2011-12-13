{-# OPTIONS -W #-}

module Partition (partition, main) where

import CFG (CFG, isTerminal)
import Chart (Chart(..), Cell, infixChart)
import Graph (Vertex, SCCL(..))
import Data.Maybe (fromMaybe)
import Broyden (broyden, zero)
import Data.Array.Unboxed (UArray)
import Data.Binary (decodeFile, encodeFile)
import Util (printListToFile)
import Nomial (Nomial, con, var, subst)
import Data.Array.IArray
import qualified Data.IntMap as IM
import qualified Data.Traversable as T
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

type Formula = Nomial Vertex Double -- [(Double, [Vertex])]

-- Try: let (w,m) = CFG.cfgOfMCFG (MCFGRead.mcfgFromFile "copy.abba.mcfg") in partition 1000 m (Graph.sccL (CFG.graphOfCFG m))

main :: IO ()
main = do
  theCFG <- decodeFile "wsj.cfg"
  theSCCs <- liftM read (readFile "wsj.sccs") {-
  let theSCCs = sccL (graphOfCFG theCFG)
  writeFile "wsj.sccs" (show theSCCs)
  -}
  let chart = infixChart [-4070 {-car/NN-}, -9756 {-dealer/NN-}]
  let (thePartitionList, thePartition) = partition chart 1000 theCFG theSCCs
  printListToFile "wsj.car-dealer.partitionlist" thePartitionList
  encodeFile "wsj.car-dealer.partition" thePartition

-- Convert a CFG RHS into a Formula by partially evaluating the RHS with
-- respect to the given chart, the given cell in the chart, and the given
-- partition values already computed for other cells in the chart.
interp :: Chart -> Cell -> Array (Int, Vertex) Double -> [Vertex] -> Formula
interp Chart{epsilon=epsilon,
             terminal=terminal,
             splits=splits,
             indexOfCell=indexOfCell} here known =
  interp' here where
  interp' cell [] = con (epsilon cell)
  interp' cell [v] | isTerminal v = con (terminal cell v)
                   | cell == here = var v
                   | otherwise    = con (known ! (indexOfCell cell, v))
  interp' cell (v:vs) = sum [ interp' l [v] * interp' r vs
                            | (l, r) <- splits cell ]

-- The vector-to-vector function whose least root we want.  The arguments
-- (curried so as to cache temporary tables) are: the CFG, the known
-- partition values, the chart, the cell to work on in the chart, the
-- strongly connected component to work on, and the current estimates.  The
-- return value is a table of the current estimates along with the
-- difference vector between the new estimates and the current estimates.
iteration :: CFG -> Array (Int, Vertex) Double -> Chart -> Cell -> SCCL ->
             UArray Int Double -> (IM.IntMap Double, UArray Int Double)
iteration cfg known chart here =
  let cfg' :: Array Vertex Formula
      cfg' = fmap (sum . map terp) cfg
        where terp (wt,rhs) = con wt * interp chart here known rhs

      here' :: Int
      here' = indexOfCell chart here
  in
  \SCCL{members=members,nonlinking=nonlinking} ->
  let linking :: IM.IntMap (Bool, Formula)
      linking = IM.fromList
        [ (i, (linking, cfg' ! i))
        | (is, linking) <- [(members, True), (nonlinking, False)]
        , i <- is ]
  in
  \olds ->
  let diff    :: [Double]
      current :: IM.IntMap Double
      (current, diff) = runWriter (evalStateT (T.mapM go linking)
                                  (fst (bounds olds)))

      go :: (Bool, Formula) -> StateT Int (Writer [Double]) Double
      go (True , fml) = return (recalc fml)
      go (False, fml) = do old <- gets (olds !)
                           modify succ
                           tell [recalc fml - old]
                           return old
      -- go (False, fml) = StateT (\i -> let old = olds ! i in
      --                                 writer ((old, succ i), [recalc fml - old]))

      recalc :: Formula -> Double
      recalc = subst lookup

      lookup :: Vertex -> Double
      lookup i = fromMaybe (known ! (here',i)) (IM.lookup i current)
  in
  (current, listArray (bounds olds) diff)

-- Main function for computing the partition function.  The arguments are: the
-- maximum number of iterations for each strongly connected component, the MCFG
-- map, and the list of strongly connected components as computed by
-- Graph.sccL.CFG.graphOfCFG.  The return values are the nonterminals and their
-- Z values, in the form of a list (sorted in computation order) and an array
-- (boxed for lazy evaluation).
partition :: Chart -> Int -> CFG -> [SCCL] ->
             ([((Int, Vertex), Double)], Array (Int, Vertex) Double)
partition chart stepsMax cfg cs = (list, known) where
  known = array ((0, size chart - 1) `boundsProduct` bounds cfg) list
  list = do
    cellIndex <- [0 .. size chart - 1]
    let cell = cellOfIndex chart cellIndex
    let iter = iteration cfg known chart cell
    c@SCCL{members=members,nonlinking=nonlinking} <- cs
    let iter_c = iter c
        init = zero (1, length nonlinking)
        (steps, solution) = broyden (snd . iter_c) stepsMax init
        final | null nonlinking  = (fst (iter_c undefined) IM.!)
              | steps < stepsMax = (fst (iter_c solution ) IM.!)
              | otherwise        = error "did not converge"
    [ ((cellIndex, i), final i) | i <- members ]
  boundsProduct :: (Ix a, Ix c) => (a,a) -> (c,c) -> ((a,c),(a,c))
  boundsProduct (a,b) (c,d) = ((a,c),(b,d))

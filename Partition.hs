{-# OPTIONS -W #-}

module Partition (partition, main) where

import CFG (CFG, isNonterminal)
import Graph (Vertex, SCCL(..))
import Data.Maybe (fromMaybe)
import Broyden (broyden, zero)
import Data.Array.Unboxed (UArray)
import Data.Binary (decodeFile)
import Util (printListToFile)
import Data.Array.IArray
import qualified Data.Array as A
import qualified Data.IntMap as IM
import qualified Data.Traversable as T
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

type Formula = [(Double, [Vertex])]

-- Try: let (w,m) = CFG.cfgOfMCFG (MCFGRead.mcfgFromFile "copy.abba.mcfg") in partition 1000 m (Graph.sccL (CFG.graphOfCFG m))

main :: IO ()
main = do
  theCFG <- decodeFile "wsj.cfg"
  theSCCs <- liftM read (readFile "wsj.sccs") {-
  let theSCCs = sccL (graphOfCFG theCFG)
  writeFile "wsj.sccs" (show theSCCs)
  -}
  let (thePartitionList, thePartition) = partition 1000 theCFG theSCCs
  printListToFile "wsj.partitionlist" thePartitionList
  printListToFile "wsj.partition" (elems thePartition)

-- The vector-to-vector function whose least root we want.  The arguments
-- (curried so as to cache temporary tables) are: the MCFG map, the known
-- partition values, the strongly connected component to work on, and the
-- current estimates.  The return value is a table of the current estimates
-- along with the difference vector between the new estimates and the current
-- estimates.
iteration :: CFG -> Array Vertex Double -> SCCL ->
             UArray Int Double -> (IM.IntMap Double, UArray Int Double)
iteration cfg =
  let cfg' = fmap (map (fmap (filter isNonterminal))) cfg
  in
  \known SCCL{members=members,nonlinking=nonlinking} ->
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
      recalc fml = sum [ weight * product (map lookup children)
                       | (weight, children) <- fml ]

      lookup :: Vertex -> Double
      lookup i = fromMaybe (known ! i) (IM.lookup i current)
  in
  (current, listArray (bounds olds) diff)

-- Main function for computing the partition function.  The arguments are: the
-- maximum number of iterations for each strongly connected component, the MCFG
-- map, and the list of strongly connected components as computed by
-- Graph.sccL.CFG.graphOfCFG.  The return values are the nonterminals and their
-- Z values, in the form of a list (sorted in computation order) and an array
-- (boxed for lazy evaluation).
partition :: Int -> CFG -> [SCCL] -> ([(Vertex, Double)], Array Vertex Double)
partition stepsMax cfg cs = (list, known) where
  iter = iteration cfg known
  known = array (bounds cfg) list
  list = do
    c@SCCL{members=members,nonlinking=nonlinking} <- cs
    let iter_c = iter c
        init = zero (1, length nonlinking)
        (steps, solution) = broyden (snd . iter_c) stepsMax init
        final | null nonlinking  = (fst (iter_c undefined) IM.!)
              | steps < stepsMax = (fst (iter_c solution ) IM.!)
              | otherwise        = error "did not converge"
    [ (i, final i) | i <- members ]


{-# OPTIONS -W #-}

module Partition (partition) where

import CFG (CFG, isNonterminal)
import Graph (Graph, Vertex, SCCL(..), sccL)
import Data.Maybe (fromMaybe)
import qualified Data.Array as A
import qualified Data.IntMap as IM
import qualified Numeric.GSL as G
import qualified Numeric.Container as C

type Formula = [(Double, [Vertex])]

-- Try: partition 1e-20 1000 (snd (CFG.cfgOfMCFG (MCFGRead.mcfgFromFile "copy.abba.mcfg"))) A.! 0

-- Turn a CFG into a graph of nonterminal usage
graphOfCFG :: CFG -> Graph
graphOfCFG = fmap (filter isNonterminal . concatMap snd)

-- The vector-to-vector function whose least root we want.  The arguments
-- (curried so as to cache temporary tables) are: the MCFG map, the known
-- partition values, the strongly connected component to work on, and the
-- current estimates.  The return value is a table of the current estimates
-- along with the difference vector between the new estimates and the current
-- estimates.
iteration :: CFG -> A.Array Vertex Double -> SCCL ->
             [Double] -> (IM.IntMap Double, [Double])
iteration cfg =
  let cfg' = fmap (map (fmap (filter isNonterminal))) cfg
  in
  \known SCCL{members=members,nonlinking=nonlinking} ->
  let linking :: IM.IntMap (Bool, Formula)
      linking = IM.fromList
        [ (i, (linking, cfg' A.! i))
        | (is, linking) <- [(members, True), (nonlinking, False)]
        , i <- is ]
  in
  \olds ->
  let diff    :: [Double]
      current :: IM.IntMap Double
      (([], diff), current) = IM.mapAccum go (olds, []) linking

      go :: ([Double], [Double]) -> (Bool, Formula) ->
           (([Double], [Double]), Double)
      go state           (True ,fml) = (state, recalc fml)
      go (old:olds,diff) (False,fml) = ((olds, recalc fml - old : diff), old)

      recalc :: Formula -> Double
      recalc fml = sum [ weight * product (map lookup children)
                       | (weight, children) <- fml ]

      lookup :: Vertex -> Double
      lookup i = fromMaybe (known A.! i) (IM.lookup i current)
  in
  (current, reverse diff)

-- Main function for computing the partition function.  The arguments are:
-- the accuracy desired, the maximum number of iterations for each strongly
-- connected component, and the MCFG map.  The return values are: the list
-- of strongly connected components, the list of nonterminals and their Z
-- values, and this last list tabulated as an array.
partition :: Double -> Int -> CFG ->
             ([SCCL], [(Vertex, Double)], A.Array Vertex Double)
partition residual duration cfg = (cs, list, known) where
  iter = iteration cfg known
  known = A.array (A.bounds cfg) list
  cs = sccL (graphOfCFG cfg)
  list = do
    c@SCCL{members=members,nonlinking=nonlinking} <- cs
    let iter_c = iter c
        (solution, path) = G.root G.Broyden residual duration
                             (snd . iter_c) [0 | _ <- nonlinking]
        final | null nonlinking        = (fst (iter_c []      ) IM.!)
              | C.rows path < duration = (fst (iter_c solution) IM.!)
              | otherwise              = \i -> error "did not converge"
    [ (i, final i) | i <- members ]


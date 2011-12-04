module Partition where

import MCFG
import Graph (Graph, Vertex, SCCL(..), sccL)
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Numeric.GSL as G
import qualified Numeric.Container as C

type Map = M.Map Cat [(Maybe Rational, RHS Vertex)]
type Formula = [(Double, [Vertex])]

-- Try: starts (partition 1e-20 1000 (mcfgMap (MCFGRead.mcfgFromFile "copy.abba.mcfg")))

-- Turn a list of parsed MCFG rules into a map from LHS to RHSs
mcfgMap :: MCFG -> Map
mcfgMap g = m where
  m = M.unionsWith (++) (map f g)
  i cat = M.findIndex cat m
  f (weight, (lhs, Cats children stringyield))
    = M.insert lhs [(weight, Cats (map i children) stringyield)]
               (M.fromList [ (cat,[]) | cat <- children ])
  f (weight, (lhs, Term term))
    = M.singleton lhs [(weight, Term term)]

-- Take a slice of an MCFG map that consists of the start symbols(' rules)
starts :: M.Map Cat a -> M.Map Cat a
starts m = let (_, s, after)  = M.splitLookup "S" m
               (before, _, _) = M.splitLookup "T" after
           in maybe before (\a -> M.insert "S" a before) s

-- Turn an MCFG map into a graph of nonterminal usage
mcfgGraph :: Map -> Graph
mcfgGraph m = A.listArray (0, M.size m - 1)
  [ [ j | (_, Cats js _) <- rhss, j <- js ] | rhss <- M.elems m ]

-- The vector-to-vector function whose least root we want.  The arguments
-- (curried so as to cache temporary tables) are: the MCFG map, the known
-- partition values, the strongly connected component to work on, and the
-- current estimates.  The return value is a table of the current estimates
-- along with the difference vector between the new estimates and the current
-- estimates.
iteration :: Map -> A.Array Vertex Double -> SCCL ->
             [Double] -> (IM.IntMap Double, [Double])
iteration m =
  let m' :: M.Map Cat Formula
      m' = M.map (\rhss -> [ (fromRational (fromJust weight),
                              case rhs of Cats children _ -> children
                                          _               -> [])
                           | (weight, rhs) <- rhss ])
                 m
  in
  \known SCCL{members=members,nonlinking=nonlinking} ->
  let linking :: IM.IntMap (Bool, Formula)
      linking = IM.fromList
        [ (i, (linking, snd (M.elemAt i m')))
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
-- connected component, and the MCFG map.  The return value is a map from
-- nonterminals to their Z values.
partition :: Double -> Int -> Map -> M.Map Cat Double
partition residual duration m = result where
  ([], result) = M.mapAccum (\(x:xs) _ -> (xs,x)) (A.elems known) m
  iter = iteration m known
  known = A.array (0, M.size m - 1) (do
    c@SCCL{members=members,nonlinking=nonlinking} <- sccL (mcfgGraph m)
    let iter_c = iter c
        (solution, path) = G.root G.Broyden residual duration
                             (snd . iter_c) [0 | _ <- nonlinking]
        final | null nonlinking        = (fst (iter_c []      ) IM.!)
              | C.rows path < duration = (fst (iter_c solution) IM.!)
              | otherwise              = \i -> error "did not converge"
    [ (i, final i) | i <- members ])


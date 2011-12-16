{-# OPTIONS -W #-}

module CFG (
    CFG, Vertex, aCFG,
    graphOfCFG,
    isNonterminal, isTerminal,
    cfgOfMCFG
) where

import MCFG (MCFG, Rule(Rule), RHS(Cats, Term), MapMCFG(MapMCFG), mapmcfgOfRule)
import Reduce (mapReduce)
import Data.Graph (Graph, Vertex)
import Data.Bifunctor (first)
import Control.Arrow ((&&&))
import qualified Data.Array as A
import qualified Data.Map as M

-- Probabilistic context-free grammars in a compact format: negative numbers
-- are terminals, non-negative numbers are non-terminals, and zero is the
-- start symbol.
type CFG = A.Array Vertex [(Double, [Vertex])]

aCFG :: CFG -- a deficient example
aCFG = A.listArray (0,0) [[(1/3, [-1]), (1/3, [0])]]

-- Turn a CFG into a graph of nonterminal usage
graphOfCFG :: CFG -> Graph
graphOfCFG = fmap (filter isNonterminal . concatMap snd)

-- Convenient discrimination on symbols represented as numbers
isNonterminal, isTerminal :: Vertex -> Bool
isNonterminal = (>= 0)
isTerminal    = (< 0)

-- Turn a list of parsed MCFG rules into an array of terminals and an array of
-- nonterminals, the latter array being in the compact CFG format
cfgOfMCFG :: (Ord term) => MCFG String term -> (A.Array Vertex term, CFG)
cfgOfMCFG g =
  (A.listArray (1, M.size words) (M.keys words),
   A.listArray (0, M.size mapmcfg - 1)
     [ [ (fromRational wt,
          case rhs of Cats children _ -> map (`M.findIndex` mapmcfg) children
                      Term term       -> [-1 - (`M.findIndex` words) term])
       | (Just wt, rhs) <- rhss ]
     | rhss <- M.elems mapmcfg ])
 where
  (MapMCFG mapmcfg, words) =
    mapReduce (mapmcfgOfRule &&& termOfRule) (map (first starts) g)
  termOfRule (Rule _ _ (Term term)) = M.singleton term ()
  termOfRule _                      = M.empty
  starts ('S':cat) = ' ':cat -- Put the start symbol first
  starts cat       = cat

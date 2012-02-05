{-# OPTIONS -W #-}
{-# LANGUAGE ExistentialQuantification #-}

module CFG (
    CFG, Vertex, Rule(..), RHS(..), aCFG,
    graphOfCFG,
    isNonterminal, isTerminal,
    Appearance(..), Consistency(..), ruleConsistency,
    Step(..), Derivation, Tree(..), Forest,
    Portray, CFGYield(..), portrayCFG, concatMap'
) where

import Data.Bifunctor (Bifunctor(bimap))
import Data.Binary (Binary(get, put))
import Data.Graph (Graph, Vertex)
import Data.List (foldl1')
import Data.Monoid (Monoid(mempty, mappend, mconcat))
import Data.Tree (Tree(..), Forest)
import Control.DeepSeq (deepseq, NFData(rnf))
import Control.Monad (liftM2, liftM3)
import qualified Data.Array as A
import qualified Data.Set as S

-- Probabilistic context-free grammars in a compact format: negative numbers
-- are terminals, non-negative numbers are non-terminals, and zero is the
-- start symbol.  Each production comes with a specification of its yield.

type CFG  prob     yield = A.Array Vertex [RHS prob Vertex yield]
data Rule prob cat yield = Rule cat (RHS prob cat yield)
  deriving (Eq, Ord)
data RHS  prob cat yield = RHS { prob     :: prob
                               , children :: [cat]
                               , yield    :: yield }
  deriving (Eq, Ord, Show)

aCFG :: (Fractional prob) => CFG prob () -- a deficient example
aCFG = A.listArray (0,0) [[RHS (1/3) [-1] (), RHS (1/3) [0] ()]]

instance Functor (Rule prob cat) where
  fmap f (Rule lhs rhs) = Rule lhs (fmap f rhs)

instance Functor (RHS prob cat) where
  fmap f rhs@RHS{yield=y} = rhs{yield=f y}

instance Bifunctor (Rule prob) where
  bimap f g (Rule lhs rhs) = Rule (f lhs) (bimap f g rhs)

instance Bifunctor (RHS prob) where
  bimap f g rhs@RHS{children=cs,yield=y} = rhs{children=map f cs,yield=g y}

instance (NFData p, NFData c, NFData y) => NFData (Rule p c y) where
  rnf (Rule lhs rhs) = rnf lhs `seq` rnf rhs

instance (NFData p, NFData c, NFData y) => NFData (RHS p c y) where
  rnf (RHS prob children yield) = rnf prob `seq` rnf children `seq` rnf yield

instance (Binary p, Binary c, Binary y) => Binary (Rule p c y) where
  put (Rule lhs rhs) = put lhs >> put rhs
  get                = liftM2 Rule get get

instance (Binary p, Binary c, Binary y) => Binary (RHS p c y) where
  put (RHS prob children yield) = put prob >> put children >> put yield
  get                       = liftM3 RHS get get get

-- Turn a CFG into a graph of nonterminal usage

graphOfCFG :: CFG prob yield -> Graph
graphOfCFG = fmap (filter isNonterminal . concatMap children)

-- Convenient discrimination on symbols represented as numbers

isNonterminal, isTerminal :: Vertex -> Bool
isNonterminal = (>= 0)
isTerminal    = (< 0)

-- Find nonterminals that are used but not defined

data Appearance = Defined | Used
  deriving (Eq, Ord, Show)

data Consistency cat = Consistency { defined, used :: S.Set cat }
  deriving (Eq, Ord, Show)

instance (NFData cat) => NFData (Consistency cat) where
  rnf (Consistency d u) = rnf d `seq` rnf u

instance (Ord cat, NFData cat) => Monoid (Consistency cat) where
  mempty = Consistency { defined = S.empty, used = S.empty }
  mappend c1 c2 = c1 `deepseq` c2 `deepseq` Consistency {
    defined = S.union (defined c1) (defined c2),
    used    = S.union (used c1 S.\\ defined c2) (used c2 S.\\ defined c1)
  }
  mconcat [] = mempty
  mconcat cs = foldl1' mappend cs

ruleConsistency :: Ord cat => Rule prob cat term -> Consistency cat
ruleConsistency (Rule lhs RHS{children=rhs}) =
  Consistency {
    defined = S.singleton lhs,
    used = S.delete lhs (S.fromList rhs)
  }

-- CFG derivations and their portrayal using yield information

data Step yield = Step Vertex yield
type Derivation yield = Tree (Step yield)

type Portray yield = (Int {-positive-} -> String) -> Derivation yield -> String
data CFGYield = forall yield. CFGYield (CFG Double yield) (Portray yield)

-- We generate derivations both from CFGs (of the type (CFG Double ()), encoded
-- to the file wsj.cfg by Grammar.main) and from MCFGs (of the type (CFG Double
-- [[Part String]]), converted from a .wmcfg file by MCFG.cfgOfMCFG) in the
-- same way, but these derivations need to be portrayed as text differently
-- because only the latter come with yield information.

portrayCFG :: Portray ()
portrayCFG wordOfIndex = portray where
  portray :: Derivation () -> String
  portray (Node (Step vertex _) children)
    | isTerminal vertex = wordOfIndex (-vertex)
    | otherwise = concatMap' portray children

concatMap' :: (a -> String) -> [a] -> String -- concatMap with spaces & brackets
concatMap' f strs = case filter (not . null) (map f strs) of
                    results@(_:_:_) -> "[" ++ unwords results ++ "]"
		    results         -> concat results

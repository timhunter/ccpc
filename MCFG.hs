{-# OPTIONS -W #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MCFG (
    MCFG, Rule(..), RHS(..), Appearance(..),
    Consistency(..), ruleConsistency,
    MapMCFG(..), mapmcfgOfRule
) where

import Data.List
import Data.Monoid
import Data.Bifunctor
import Data.Ratio (numerator, denominator)
import Control.DeepSeq
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M

-- Multiple context-free grammars in mcfgcky2 format

type MCFG cat term = [Rule cat term]
data Rule cat term = Rule (Maybe Rational) cat (RHS cat term) deriving (Eq, Ord)
data RHS  cat term = Cats [cat] [[(Int, Int)]] | Term term    deriving (Eq, Ord)

instance (NFData cat, NFData term) => NFData (Rule cat term) where
  rnf (Rule wt lhs rhs) = rnf wt `seq` rnf lhs `seq` rnf rhs

instance (NFData cat, NFData term) => NFData (RHS cat term) where
  rnf (Cats children stringyield) = rnf children `seq` rnf stringyield
  rnf (Term term)                 = rnf term

instance (Show cat, Show term) => Show (Rule cat term) where
  show (Rule wt lhs rhs) =
    maybe "" (\prob -> show (numerator prob) ++ "/" ++
                       show (denominator prob) ++ " ") wt ++
    show lhs ++ " --> " ++ show rhs

instance (Show cat, Show term) => Show (RHS cat term) where
  show (Cats [] [[]]) = "\"\""
  show (Cats children stringyield) =
    intercalate " " (map show children ++
                     [ "[" ++ (if null component then "Epsilon" else
  		             intercalate ";" [ show i ++ "," ++ show j
  			                     | (i,j) <- component ]) ++ "]"
  		   | component <- stringyield ])
  show (Term term) = "\"" ++ show term ++ "\""

instance Functor (Rule cat) where
  fmap f (Rule wt lhs rhs) = Rule wt lhs (fmap f rhs)

instance Functor (RHS cat) where
  fmap _ (Cats children stringyield) = Cats children stringyield
  fmap f (Term x)                    = Term (f x)

instance Bifunctor Rule where
  bimap f g (Rule wt lhs rhs) = Rule wt (f lhs) (bimap f g rhs)

instance Bifunctor RHS where
  bimap f _ (Cats children stringyield) = Cats (map f children) stringyield
  bimap _ g (Term term)                 = Term (g term)

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

ruleConsistency :: Ord cat => Rule cat term -> Consistency cat
ruleConsistency (Rule _wt lhs rhs) =
  Consistency {
    defined = S.singleton lhs,
    used = case rhs of Term _ -> S.empty
                       Cats rhs _ -> S.delete lhs (S.fromList rhs)
  }

-- Collate rules by LHS

newtype MapMCFG wt cat term = MapMCFG (M.Map cat [(wt, RHS cat term)])
  deriving (Eq, Ord, Show, NFData)

instance (Ord cat) => Monoid (MapMCFG wt cat term) where
  mempty = MapMCFG M.empty
  mappend (MapMCFG g1) (MapMCFG g2) = MapMCFG (M.unionWith (++) g1 g2)

mapmcfgOfRule :: Rule cat term -> MapMCFG (Maybe Rational) cat term
mapmcfgOfRule (Rule wt lhs rhs) = MapMCFG (M.singleton lhs [(wt, rhs)])


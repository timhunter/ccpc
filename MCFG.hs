{-# OPTIONS -w #-}

module MCFG (
    MCFG, Rule, RHS(..), Appearance(..),
    Consistency(..), ruleConsistency
) where

import Data.List
import Data.Monoid
import Control.DeepSeq
import Control.Monad
import qualified Data.Set as S

type MCFG cat term = [Rule cat term]
type Rule cat term = (Maybe Rational, (cat, RHS cat term))
data RHS  cat term = Cats [cat] [[(Int, Int)]] | Term term
  deriving (Eq, Ord, Show)

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
ruleConsistency (wt, (lhs, rhs)) =
  Consistency {
    defined = S.singleton lhs,
    used = case rhs of Term _ -> S.empty
                       Cats rhs _ -> S.delete lhs (S.fromList rhs)
  }


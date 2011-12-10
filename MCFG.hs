{-# OPTIONS -w #-}

module MCFG (
    MCFG, Rule, RHS(..), Appearance(..),
    Consistency, ruleConsistency, consistency
) where

import Data.List
import Data.Monoid
import Control.DeepSeq
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

type MCFG cat term = [Rule cat term]
type Rule cat term = (Maybe Rational, (cat, RHS cat term))
data RHS  cat term = Cats [cat] [[(Int, Int)]] | Term term
  deriving (Eq, Ord, Show)

-- Find definitions that don't sum up to 1
--  and uses that refer to undefined nonterminals

data Appearance cat = DefinedTo (Maybe Rational) | UsedBy (S.Set cat)
  deriving (Eq, Ord, Show)

instance (NFData cat) => NFData (Appearance cat) where
  rnf (DefinedTo x) = rnf x
  rnf (UsedBy    x) = rnf x

newtype Consistency cat = Consistency (M.Map cat (Appearance cat))
  deriving (Eq, Ord, Show)

instance (NFData cat) => NFData (Consistency cat) where
  rnf (Consistency x) = rnf x

instance (Ord cat, NFData cat) => Monoid (Consistency cat) where
  mempty = Consistency M.empty
  mappend (Consistency c1) (Consistency c2) =
    c1 `deepseq` c2 `deepseq` Consistency (M.unionWith f c1 c2)
    where f (DefinedTo wt1) (DefinedTo wt2) = DefinedTo (liftM2 (+) wt1 wt2)
          f (UsedBy    cs1) (UsedBy    cs2) = UsedBy    (S.union    cs1 cs2)
          f (DefinedTo wt ) (UsedBy   _cs ) = DefinedTo wt
          f (UsedBy   _cs ) (DefinedTo wt ) = DefinedTo wt
  mconcat [] = mempty
  mconcat cs = foldl1' mappend cs

ruleConsistency :: Ord cat => Rule cat term -> Consistency cat
ruleConsistency (wt, (lhs, Term _)) =
  Consistency (M.singleton lhs (DefinedTo wt))
ruleConsistency (wt, (lhs, Cats rhs _)) =
  Consistency (M.insert    lhs (DefinedTo wt)
    (M.fromList [ (cat, appearance) | cat <- rhs ]))
  where appearance = UsedBy (S.singleton lhs)

consistency :: Ord cat => Consistency cat -> Consistency cat
consistency (Consistency c) = Consistency (M.filter f c)
  where f (DefinedTo (Just 1)) = False
        f _                    = True


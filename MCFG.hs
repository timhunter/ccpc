module MCFG where

type MCFG cat term = [(Maybe Rational, (cat, RHS cat term))]
data RHS  cat term = Cats [cat] [[(Int, Int)]] | Term term
    deriving (Eq, Ord, Show)


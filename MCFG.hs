module MCFG where

type MCFG    = [(Maybe Rational, (Cat, RHS Cat))]
type Cat     = String
type Term    = String
data RHS cat = Cats [cat] [[(Int, Int)]] | Term Term
    deriving (Eq, Ord, Show)


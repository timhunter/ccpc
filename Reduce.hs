{-# OPTIONS -W #-}

module Reduce (reduce, mapReduce) where

import Data.Monoid
import Data.List (foldl')
import Control.Parallel (par)

-- MapReduce over a long list

reduce :: Monoid a => [a] -> a
reduce []       = mempty
reduce [x]      = x
reduce (x:y:xs) = reduce (reverse (map snd
                    (foldl' (shift 0) [(1, mappend x y)] xs)))

shift :: Monoid a => Int -> [(Int,a)] -> a -> [(Int,a)]
shift n _          x | (case compare n 9 of { LT -> id; EQ -> par x; GT ->
                        if n >= 12 then seq x else id }) False = undefined
shift n []         x = [(n,x)]
shift n ((0,y):ys) x = shift (succ n) ys (mappend y x)
shift n ((m,y):ys) x = (n,x):(pred m,y):ys

mapReduce :: Monoid a => (b -> a) -> [b] -> a
mapReduce f = reduce . map f

-- Testing

data Tree = Leaf Int | Branch Tree Tree deriving (Eq, Ord, Show)
instance Monoid Tree where
  mempty  = Leaf 0
  mappend = Branch


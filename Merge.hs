{-# OPTIONS -W #-}

module Merge (
    Forest,
    toList,
    toListBy,
    empty,
    singleton,
    fromAscList,
    crossWithAscList
) where

import Data.Tree (Tree(Node, rootLabel), Forest)
import Data.Function (on)
import qualified PriorityQueue.PriorityQueue as Q
import Test.QuickCheck
import Data.List (sort)
import Control.Monad (liftM2)

toList :: (Ord a) => Forest a -> [a]
toList = toListBy compare

toListBy :: (a -> a -> Ordering) -> Forest a -> [a]
toListBy cmp ts = go (Q.fromListBy cmp' ts) where
  go q | Q.isEmpty q = []
       | otherwise   = seq q' (x : go (foldr (Q.insertBy cmp') q' ts))
                       where q' = Q.deleteMinBy cmp' q
                             Node x ts = Q.findMin q
  cmp' = cmp `on` rootLabel

empty :: Forest a
empty = []

singleton :: a -> Forest a
singleton x = [Node x []]

fromAscList :: [a] -> Forest a
fromAscList []     = []
fromAscList (x:xs) = [Node x (fromAscList xs)]

crossWithAscList :: (a -> b -> c) -> [a] -> Forest b -> Forest c
crossWithAscList _ []     = const []
crossWithAscList f (x:xs) = g where
  g = map (\(Node y ts) -> Node (x `f` y) (fromAscList (map (`f` y) xs) ++ g ts))

main :: IO ()
main = do
  quickCheck (null (toList empty :: [Double]))
  quickCheck (\x -> [x]
                 == (toList (singleton x) :: [Double]))
  quickCheck (\xs -> sort xs
                  == (toList (xs >>= singleton) :: [Double]))
  quickCheck (\xs -> sort xs
                  == (toList (fromAscList (sort xs)) :: [Double]))
  quickCheck (conjoin
    [ \xs ys -> sort (liftM2 f xs ys)
             == (toList (crossWithAscList f (sort xs) (fromAscList (sort ys))) :: [Double])
    | f <- [(+), (*) `on` (1.01**), const, const id] ])
  quickCheck (conjoin
    [ \xs ys zs -> sort (liftM2 f xs (liftM2 f ys zs))
             == (toList (crossWithAscList f (sort xs) (
                         crossWithAscList f (sort ys) (fromAscList (sort zs)))) :: [Double])
    | f <- [(+), (*) `on` (1.01**), const, const id] ])

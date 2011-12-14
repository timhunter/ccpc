{-# OPTIONS -W #-}

module Graph (module Data.Graph, SCCL(..), sccL) where

import Data.Array
import Data.Graph
import Data.Binary (Binary(put, get))
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Control.Monad
import Control.Monad.ST

-- Compute the strongly connected components of a directed graph,
-- and designate zero or more vertices in each component as nonlinking
-- \citep{nederhof-computing}.  The computed components are topologically
-- sorted, as with Data.Graph.stronglyConnComp.

data SCCL = SCCL { members, nonlinking :: [Vertex] } deriving (Show, Read)

instance Binary SCCL where
  put (SCCL m n) = do put m
                      put n
  get = do m <- get
           n <- get
           return (SCCL m n)

sccL :: Graph -> [SCCL]
sccL g = map (`decode` SCCL [] []) (dfs' g (topSort (transposeG g)))

data Tree' a = Node' a (Forest' a) | Nonlink a
type Forest' a = [Tree' a]

type Epoch = Int

-- For Partition.iterationGen to work, this traversal must be postfix!
-- This is unlike the decode function in Data.Graph.stronglyConnCompR
decode :: Tree' Vertex -> SCCL -> SCCL
decode (Node' v ts) c =
  foldr decode SCCL{members = v : members c, nonlinking = nonlinking c} ts
decode (Nonlink v) c =
  SCCL{members = members c, nonlinking = v : nonlinking c}

dfs' :: Graph -> [Vertex] -> Forest' Vertex
dfs' g vs = prune' (bounds g) (map (generate g) vs)

generate :: Graph -> Vertex -> Tree Vertex
generate g v = Node v (map (generate g) (g!v))

prune' :: Bounds -> Forest Vertex -> Forest' Vertex
prune' bnds ts = runST (do
  m <- newArray bnds (maxBound::Epoch)
  liftM concat (zipWithM (\epoch t -> chop' m epoch [t]) [1..] ts))

chop' :: STArray s Vertex Epoch -> Epoch ->
         Forest Vertex -> ST s (Forest' Vertex)
chop' _ _ [] = return []
chop' m epoch (Node v ts : us) = do
  visited <- readArray m v
  case compare visited epoch of
    LT -> chop' m epoch us
    EQ -> do writeArray m v (minBound::Epoch)
             bs <- chop' m epoch us
             return (Nonlink v : bs)
    GT -> do writeArray m v epoch
             as <- chop' m epoch ts
             bs <- chop' m epoch us
             return (Node' v as : bs)


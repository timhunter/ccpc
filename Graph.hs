module Graph (module Data.Graph, SCCL(..), sccL) where

import Data.Array
import Data.Graph
import Data.Tree (Tree(Node), Forest)
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Control.Monad
import Control.Monad.ST

-- Compute the strongly connected components of a directed graph,
-- and designate zero or more vertices in each component as nonlinking
-- \citep{nederhof-computing}.  The computed components are topologically
-- sorted, as with Data.Graph.stronglyConnComp.

data SCCL = SCCL { members, nonlinking :: [Vertex] } deriving Show

sccL :: Graph -> [SCCL]
sccL g = map (`decode` SCCL [] []) (dfs' g (topSort (transposeG g)))

data Tree' a = Node' a (Forest' a) | Nonlink a
type Forest' a = [Tree' a]

type Epoch = Int

decode :: Tree' Vertex -> SCCL -> SCCL
decode (Node' v ts) c = SCCL (v : members c') (nonlinking c')
  where c' = foldr decode c ts
decode (Nonlink v)  c = SCCL (members c) (v : nonlinking c)

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
chop' m epoch [] = return []
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


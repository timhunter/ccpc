module CFG (CFG, Vertex) where

import qualified Data.Array as A
import Data.Graph (Vertex)

-- Probabilistic context-free grammars in a compact format: negative numbers
-- are terminals, non-negative numbers are non-terminals, and zero is the
-- start symbol.
type CFG = A.Array Vertex [(Double, [Vertex])]

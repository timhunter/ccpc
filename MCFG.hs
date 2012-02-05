{-# OPTIONS -W #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MCFG (
    MCFG, MRule, MRHS,
    Part(Child, Term), isChild, isTerm, fromChild, fromTerm,
    cfgOfMCFG, portrayMCFG
) where

import CFG
import Reduce (mapReduce)
import Util (MonoidMap(MonoidMap))
import Data.Bifunctor (first)
import Data.Binary (Binary(get, put), getWord8, putWord8)
import Data.List (intercalate)
import Data.Ratio (numerator, denominator)
import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM, liftM2)
import qualified Data.Array as A
import qualified Data.Map as M

-- Multiple context-free grammars in mcfgcky2 format

type MCFG  cat term = [MRule cat term]
type MRule cat term = Rule (Maybe Rational) cat [[Part term]]
type MRHS  cat term = RHS  (Maybe Rational) cat [[Part term]]
data Part      term = Child Int Int | Term term deriving (Eq, Ord)

isChild, isTerm :: Part term -> Bool
isChild (Child _ _) = True
isChild _           = False
isTerm  (Term _)    = True
isTerm  _           = False

fromChild :: Part term -> (Int, Int)
fromChild (Child i j) = (i, j)
fromChild _           = error "MCFG.fromChild: Term"

fromTerm :: Part term -> term
fromTerm (Term term) = term
fromTerm _           = error "MCFG.fromTerm: Child"

instance Functor Part where
  fmap _ (Child i j) = Child i j
  fmap f (Term term) = Term (f term)

instance (NFData term) => NFData (Part term) where
  rnf (Child i j) = rnf i `seq` rnf j
  rnf (Term term) = rnf term

instance (Binary term) => Binary (Part term) where
  put (Child i j) = putWord8 0 >> put i >> put j
  put (Term term) = putWord8 1 >> put term
  get = do tag <- getWord8
           case tag of 0 -> liftM2 Child get get
                       1 -> liftM Term get
                       _ -> fail "instance Binary (MCFG.Part term): no parse"

instance (Show term) => Show (Part term) where
  showsPrec _ (Child i j) = showsPrec 0 i . showChar ',' . showsPrec 0 j
  showsPrec _ (Term term) = showChar '"' . showsPrec 0 term . showChar '"'
  showList xs = showChar '[' . case xs of [] -> showString "Epsilon]"
                                          x:xs -> showsPrec 0 x . s xs
    where s [] = showChar ']'
          s (x:xs) = showChar ';' . showsPrec 0 x . s xs

instance (Show cat, Show term) => Show (MRule cat term) where
  show (Rule lhs (RHS prob children yield)) = concat
    [ case prob of
      Nothing -> ""
      Just p -> show (numerator p) ++ "/" ++ show (denominator p) ++ " "
    , show lhs
    , " -->"
    , concat [ " " ++ show cat | cat <- children ]
    , case yield of
      [] -> ""
      [xs] | all isTerm xs
        -> " \"" ++ unwords (map (show . fromTerm) xs) ++ "\""
      _ -> " " ++ concatMap show yield ]

-- Collate a list of parsed MCFG rules into the compact CFG format

cfgOfMCFG :: (Fractional prob, Ord term) =>
             MCFG String term -> (A.Array Vertex term, CFG prob [[Part term]])
cfgOfMCFG g =
  ( A.listArray (1, M.size words) (M.keys words)
  , A.listArray (0, M.size m - 1)
    $ (map.map) (\rhs@RHS{children=cs,yield=y} -> rhs{children=
                  -- hack to coerce multiple components into one
                  [ -1 - M.findIndex term words | Term term <- concat y ] ++
                  [      M.findIndex c    m     |      c    <- cs       ]})
                (M.elems m) )
  where
    (MonoidMap m, words) = mapReduce (f . first starts) g
    f (Rule lhs rhs@RHS{prob=p, yield=y})
      = ( MonoidMap (M.singleton lhs [rhs{prob = maybe 0 fromRational p}])
        , M.fromList [ (term, ()) | Term term <- concat y ] )
    starts ('S':cat) = Left cat -- Put the start symbol first
    starts cat       = Right cat

-- Portray MCFG derivations (as opposed to CFG derivations; see CFG.portrayCFG)

portrayMCFG :: Portray [[Part String]]
portrayMCFG wordOfIndex = intercalate "\t" . portray where
  portray :: Derivation [[Part String]] -> [String]
  portray (Node (Step vertex yield) children)
    | isTerminal vertex = [wordOfIndex (-vertex)]
    | otherwise = let pcs = map portray children
                      f (Child i j) = pcs !! i !! j
                      f (Term term) = term
                  in (map.concatMap') f yield

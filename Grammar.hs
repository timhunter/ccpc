{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Grammar where

import Prelude
import Observed
import MCFG
import Data.Atom.Simple
import Data.Ratio ((%))
import Data.Map (Map, (!))
import Data.Either (partitionEithers)
import Data.Monoid
import Control.Category ((>>>))
import qualified Data.Map as M
import qualified Data.Set as S

data Prev = START | CC | PUNC | OTHER
  deriving (Eq, Ord, Show)

data Cat = Start
         | Init                       Label Lex POS
         | Bar                  Label Label Lex POS
         | Sub Side             Label Label (Maybe (Maybe Lex, POS))
         | Mod Side Subcat Prev Label Label (Maybe (Maybe Lex, POS))
  deriving (Eq, Ord)

instance Show Cat where
  show Start = "S"
  show (Init label lex pos) =
    show label ++ "(" ++ show lex ++ show pos ++ ")"
  show (Bar parent head lex pos) =
    show head ++ "^" ++ show parent ++ "(" ++ show lex ++ show pos ++ ")"
  show (Sub side parent head word) =
    show head ++ "^" ++ show parent ++ "(" ++ maybe "" (\(lex, pos) -> maybe "" show lex ++ show pos) word ++ ")" ++ show side
  show (Mod side (Subcat subcat) prev parent head word) =
    show head ++ "^" ++ show parent ++ "(" ++ maybe "" (\(lex, pos) -> maybe "" show lex ++ show pos) word ++ ")" ++ show side ++ show subcat ++ show prev

infixl 0 -->
(-->) :: Cat -> [Cat] -> (Cat, RHS Cat Lex)
lhs --> rhs = (lhs, Cats rhs [zipWith (\_ i -> (i,0)) rhs [0..]])

type Counts v = Map v Count
type Dist   v = Map v Rational

reset :: Either w w -> w
reset (Left  w) = w
reset (Right w) = w

(!?) :: (Ord k) => Either w (w, Map k a) -> k -> Either w a
Left w       !? _ = Left w
Right (w, m) !? k = maybe (Left w) Right (M.lookup k m)

data Obs k v = Obs { past :: k, future :: v, count :: Count }

class Tally k v where
  type Tallied k v
  tally :: (Ord v) => [Obs k v] -> (Counts v, Tallied k v)

instance Tally () v where
  type Tallied () v = ()
  tally os = (M.fromListWith (+) [ (v, c) | Obs () v c <- os ], ())

instance (Ord k1, Tally k2 v) => Tally (k1, k2) v where
  type Tallied (k1, k2) v = Map k1 (Counts v, Tallied k2 v)
  tally os = (M.unionsWith (+) (map fst (M.elems m)), m)
    where m = M.map tally (M.fromListWith (++)
                [ (k1, [Obs k2 v c]) | Obs (k1, k2) v c <- os ])

instance (Tally k v) => Tally (Maybe k) v where
  type Tallied (Maybe k) v = Tallied k v
  tally os = (M.unionWith (+) cNothing cJust, rJust)
    where (cNothing, ()) = tally osNothing
          (cJust, rJust) = tally osJust
          (osNothing, osJust) = partitionEithers (map f os)
          f (Obs Nothing  v c) = Left  (Obs () v c)
          f (Obs (Just k) v c) = Right (Obs k  v c)

rough :: Map a Count -> Map a Rational
rough counts = M.map (% total) counts
  where total = sum (M.elems counts)

smoothBy :: (Ord a) => (Count -> Count) ->
            Map a Rational -> Map a Count -> Map a Rational
smoothBy backoff_of_diversity coarser finer -- Bikel equation (19)
  = M.unionWith (+) (M.map (% denominator)             finer)
                    (M.map ((backoff % denominator) *) coarser)
  where total       = sum (M.elems finer)
        diversity   = fromIntegral (M.size finer)
        backoff     = backoff_of_diversity diversity
        denominator = total + backoff

smooth, smooth1 :: (Ord a) => Map a Rational -> Map a Count -> Map a Rational
smooth  = smoothBy (5 *)     -- Bikel: f_t = 0 and f_f = 5.0
smooth1 = smoothBy (const 5) -- Bikel: f_t = 5.0 and f_f = 0

top_rules :: MCFG Cat Lex
top_rules = do ((nt, pos), wt1) <- M.toList top_nt_dist
               (lex      , wt2) <- M.toList (top_w_dists ! pos ! nt)
               [(Just (wt1 * wt2), Start --> [Init nt lex pos])]

lex_rules :: MCFG Cat Lex
lex_rules = do (lex, poss) <- M.toList thePosMap
               pos <- poss
               [(Just 1, (Init pos lex pos, Term lex {- pos -}))]

head_rules :: () -> MCFG Cat Lex
head_rules () = do
  parent <- theNonts
  let m = head_dists ! parent
  (lex, poss) <- M.toList thePosMap
  pos <- poss
  let (word, dist) = case M.lookup pos (snd m) of
                       Nothing -> (Nothing, fst m)
                       Just m  -> case M.lookup lex (snd m) of
                                    Nothing -> (Just (Nothing , pos), fst m)
                                    Just m  -> (Just (Just lex, pos), m)
  (head, wt) <- M.toList dist
  let parent' = unArg parent
  [(Just wt,
    Init parent lex pos --> [Sub L parent' head word, Bar parent' head lex pos]),
   (Just 1,
    Bar parent' head lex pos --> [Init head lex pos, Sub R parent' head word])]

subcat_rules :: () -> MCFG Cat Lex
subcat_rules () = do
  ((side, parent', head'), m) <- M.toList subcat_dists
  (word, dist) <- (Nothing, fst m) : do
    (pos, m) <- M.toList (snd m)
    (Just (Nothing, pos), fst m) : do
      (lex, m) <- M.toList (snd m)
      [(Just (Just lex, pos), m)]
  (subcat, wt) <- M.toList dist
  head <- reArg head'
  [(Just wt,
    Sub side parent' head word --> [Mod side subcat START parent' head word])]

top_nt_counts :: Counts (Label, POS)
top_nt_counts = fst $ tally
  [ Obs () (head, pos) count
  | Head (Word _ pos _) (Just (parent, head, _, _)) count <- theEvents
  , parent == _TOP_ ]

top_nt_dist :: Dist (Label, POS)
top_nt_dist = rough top_nt_counts

top_w_counts :: Map POS (Counts Lex, Map Label (Counts Lex, ()))
top_w_counts = snd $ tally
  [ Obs (pos, case info of
                Just (parent, head, _, _) | parent == _TOP_ -> Just (head, ())
                _ -> Nothing)
        lex count
  | Head (Word lex pos _) info count <- theEvents ]

top_w_dists :: Map POS (Map Label (Dist Lex))
top_w_dists = M.map (uncurry (rough    >>> \d ->
              M.map (uncurry (smooth d >>> \d -> \() -> d))))
            $ top_w_counts

head_counts :: Map Nont (Counts Label,
               Map POS  (Counts Label,
               Map Lex  (Counts Label, ())))
head_counts = snd $ tally
  [ Obs (parent, (pos, (lex, ()))) head count
  | Head (Word lex pos _) (Just (parent, head, _, _)) count <- theEvents ]

head_dists :: Map Nont (Dist Label,
              Map POS  (Dist Label,
              Map Lex  (Dist Label)))
head_dists = M.map (uncurry (rough    >>> \d -> ((,) d) .
             M.map (uncurry (smooth d >>> \d -> ((,) d) .
             M.map (uncurry (smooth d >>> \d -> \() -> d))))))
           $ head_counts

subcat_counts :: Map (Side, Nont, Label) (Counts Subcat,
                 Map POS                 (Counts Subcat,
                 Map Lex                 (Counts Subcat, ())))
subcat_counts = snd $ tally
  [ Obs ((side, unArg parent, unArg head), (pos, (lex, ()))) subcat count
  | Head (Word lex pos _) (Just (parent, head, left, right)) count <- theEvents
  , (side, subcat) <- [(L, left), (R, right)] ]

subcat_dists :: Map (Side, Nont, Label) (Dist Subcat,
                Map POS                 (Dist Subcat,
                Map Lex                 (Dist Subcat)))
subcat_dists = M.map (uncurry (rough     >>> \d -> ((,) d) .
               M.map (uncurry (smooth1 d >>> \d -> ((,) d) .
               M.map (uncurry (smooth1 d >>> \d -> \() -> d))))))
             $ subcat_counts

-- Debugging

class FoldableWithKey t where
  foldMapWithKey :: Monoid m => (((k0, k), a) -> m) -> (k0, t k a) -> m

instance FoldableWithKey Map where
  foldMapWithKey f (k0,m) = mconcat [ f ((k0,k),v) | (k,v) <- M.assocs m ]

instance FoldableWithKey (,) where
  foldMapWithKey f (k0,(k,v)) = f ((k0,k),v)

head_phw   = (foldMapWithKey . foldMapWithKey . foldMapWithKey . foldMapWithKey . foldMapWithKey . foldMapWithKey)
             (\((((((((),parent),_),pos),_),lex),head),_) ->
              S.singleton (parent,head,pos,lex))
             ((),head_dists)
subcat_phw = (foldMapWithKey . foldMapWithKey . foldMapWithKey)
             (\(((((),(_,parent,head)),pos),lex),_) ->
              S.singleton (parent,head,pos,lex))
             ((),subcat_dists)


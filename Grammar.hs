{-# OPTIONS -W #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Grammar where

import Prelude hiding (lex)
import Observed hiding (main)
import MCFG
import Data.Ratio ((%))
import Data.Map (Map, (!))
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Control.Category ((>>>))
import Control.Arrow ((***))
import Control.DeepSeq (NFData(rnf), deepseq)
import Data.Foldable (foldMap)
import Control.Parallel.Strategies (parMap, rdeepseq)
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = deepseq theEvents $ print $
  -- consistency $ mconcat $ parMap rdeepseq (foldMap ruleConsistency) $
  parMap rdeepseq length $
  [top_rules, lex_rules, head_rules (), mod_rules (), npb_rules ()]

data Prev = START | CC | PUNC | OTHER
  deriving (Eq, Ord, Show)

instance NFData Prev

prevOfLabel :: Label -> Prev
prevOfLabel prev | prev == _START_                    = START
                 | prev == _CC_                       = CC
                 | prev == _COMMA_ || prev == _COLON_ = PUNC
                 | otherwise                          = OTHER

type Lex_POS = Maybe (Maybe Lex, POS)
data Cat
  = Start
  | Init                        Label Lex POS
  | Bar                   Label Label Lex POS
  | Mod    Side           Label Label Lex_POS
  | ModLex Side Label POS Label Label Lex_POS
  | NPB    Side                 Label Lex_POS
  | NPBLex Side Label POS       Label Lex_POS
                                          --- the head word          $w_h$
                                      --- the head tag               $t_h$
                                ----- the head child's nonterminal/POS $H$
                          ----- the parent node's nonterminal/POS      $P$
                      --- the new modifier's head POS            $t_{M_i}$
                ----- the new modifier's nonterminal/POS             $M_i$
           ---- whether to generate modifiers to the left or right  $side$
  deriving (Eq, Ord)

instance Show Cat where
  show Start = "S"
  show (Init label lex pos) =
    show label ++ "(" ++ show lex ++ show pos ++ ")"
  show (Bar parent head lexHead posHead) =
    show head ++ "^" ++ show parent ++
    "(" ++ show lexHead ++ show posHead ++ ")"
  show (Mod side parent head word) =
    show head ++ "^" ++ show parent ++
    "(" ++ showHeadWord word ++ ")" ++ show side
  show (ModLex side mod pos parent head word) =
    show mod ++ "(" ++ show pos ++ ")" ++ show head ++ "^" ++ show parent ++
    "(" ++ showHeadWord word ++ ")" ++ show side
  show (NPB side key word) =
    show key ++ "^NPB(" ++ showHeadWord word ++ ")" ++ show side
  show (NPBLex side mod pos key word) =
    show mod ++ "(" ++ show pos ++ ")" ++ show key ++
    "^NPB(" ++ showHeadWord word ++ ")" ++ show side

instance NFData Cat where
  rnf Start = ()
  rnf (Init label lex pos) =
    rnf label `seq` rnf lex `seq` rnf pos
  rnf (Bar parent head lex pos) =
    rnf parent `seq` rnf head `seq` rnf lex `seq` rnf pos
  rnf (Mod side parent head word) =
    rnf side `seq` rnf parent `seq` rnf head `seq` rnf word
  rnf (ModLex side mod pos parent head word) =
    rnf side `seq` rnf mod `seq` rnf pos `seq`
    rnf parent `seq` rnf head `seq` rnf word
  rnf (NPB side key word) = rnf side `seq` rnf key `seq` rnf word
  rnf (NPBLex side mod pos key word) =
    rnf side `seq` rnf mod `seq` rnf pos `seq` rnf key `seq` rnf word

showHeadWord :: Maybe (Maybe Lex, POS) -> String
showHeadWord Nothing                = ""
showHeadWord (Just (Nothing , pos)) = show pos
showHeadWord (Just (Just lex, pos)) = show lex ++ show pos

infixl 0 -->
(-->) :: Cat -> [Cat] -> (Cat, RHS Cat Lex)
lhs --> rhs = (lhs, Cats rhs [zipWith (\_ i -> (i,0)) rhs [0..]])

cons :: Side -> a -> [a] -> [a]
cons L x xs = x : xs
cons R x xs = xs ++ [x]

type Counts v = Map v Count
type Prob     = Rational
type Dist   v = Map v Prob

seek :: (Ord k, Show k) => String -> Map k a -> k -> a
seek tag m k = fromMaybe (error ("seek (" ++ tag ++ ") " ++ show k))
                         (M.lookup k m)

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

rough :: Counts a -> Dist a
rough counts = M.map ((% total) . fromIntegral) counts
  where total = fromIntegral (sum (M.elems counts))

smoothedBy :: (Ord a) => (Count -> Count) -> Dist a -> Counts a -> Dist a
smoothedBy backoffOfDiversity coarser finer
  = M.unionWith (+) (M.map (deficiency *) coarser) rescaled
  where (deficiency, rescaled) = smoothBy backoffOfDiversity finer

smoothBy :: (Ord a) => (Count -> Count) -> Counts a -> (Prob, Dist a)
smoothBy backoffOfDiversity finer
  -- Bikel equation (19):
  -- Setting backoffOfDiversity to (5 *)     gives Bikel's f_t = 0 and f_f = 5.0
  -- Setting backoffOfDiversity to (const 5) gives Bikel's f_t = 5.0 and f_f = 0
  = (fromIntegral backoff % denominator,
     M.map ((% denominator) . fromIntegral) finer)
  where total       = sum (M.elems finer)
        diversity   = M.size finer
        backoff     = backoffOfDiversity diversity
        denominator = fromIntegral (total + backoff)

smoothed :: (Ord a) => Dist a -> Counts a -> Dist a
smooth   :: (Ord a) => Counts a -> (Prob, Dist a)
smoothed = smoothedBy (5 *)
smooth   = smoothBy   (5 *)

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
  (parent, m) <- M.toList head_dists
  (lex, poss) <- M.toList thePosMap
  pos <- poss
  let (word, dist) = case M.lookup pos (snd m) of
                       Nothing -> (Nothing, fst m)
                       Just m  -> case M.lookup lex (snd m) of
                                    Nothing -> (Just (Nothing , pos), fst m)
                                    Just m  -> (Just (Just lex, pos), m)
  (head, wt) <- M.toList dist
  let cat | parent == _NPB_ = \side -> npbCat side head pos lex
          | otherwise       = \side -> Mod side parent head word
  [(Just wt, Init parent lex pos     --> [cat L, Bar parent head lex pos]),
   (Just 1,  Bar parent head lex pos --> [Init head lex pos, cat R])]

mod_rules :: () -> MCFG Cat Lex
mod_rules () = do
  ((side, parent, head), m) <- M.toList mod_nt_dists
  (wordHead, dist) <- (Nothing, fst m) :
    do (posHead, m) <- M.toList (snd m)
       (Just (Nothing, posHead), fst m) :
         do (lexHead, m) <- M.toList (snd m)
            [(Just (Just lexHead, posHead), m)]
  ((mod, pos), wt1) <- M.toList dist
  let cat              = Mod    side         parent head wordHead
      catlex wordHead  = ModLex side mod pos parent head wordHead
      catrule wordHead = (Just wt1, cat --> cons side cat [catlex wordHead])
      rulesOfDist dist = [ (Just wt2, catlex wordHead --> [Init mod lex pos])
                         | (lex, wt2) <- M.toList dist ]
  if mod == _STOP_ then [(Just wt1, cat --> [])] else do
    let m = seek "mod_w_dists" mod_w_dists pos
    case wordHead of
      Nothing ->
        catrule wordHead : rulesOfDist (fst m)
      Just (lexHead, posHead) ->
        case M.lookup (mod, side, parent, head, posHead) (snd m) of
          Nothing -> [catrule Nothing]
          Just m ->
            case lexHead of
              Nothing ->
                catrule wordHead :
                (Just (fst (fst m)), catlex wordHead --> [catlex Nothing]) :
                rulesOfDist (snd (fst m))
              Just lexHead ->
                let wordHead' = Just (Nothing, posHead) in
                case M.lookup lexHead (snd m) of
                  Nothing -> [catrule wordHead']
                  Just m ->
                    catrule wordHead :
                    (Just (fst m), catlex wordHead --> [catlex wordHead']) :
                    rulesOfDist (snd m)

npb_rules :: () -> MCFG Cat Lex
npb_rules () = do
  ((side, key), m) <- M.toList npb_nt_dists
  (wordKey, dist) <- (Nothing, fst m) :
    do (posKey, m) <- M.toList (snd m)
       (Just (Nothing, posKey), fst m) :
         do (lexKey, m) <- M.toList (snd m)
            [(Just (Just lexKey, posKey), m)]
  ((mod, pos), wt1) <- M.toList dist
  let cat              = NPB    side         key wordKey
      catlex wordKey   = NPBLex side mod pos key wordKey
      catrule wordKey  = (Just wt1, cat --> [catlex wordKey])
      rulesOfDist dist = [ (Just wt2, catlex wordKey -->
                                      cons side (npbCat side mod pos lex)
                                                [Init mod lex pos])
                         | (lex, wt2) <- M.toList dist ]
  if mod == _STOP_ then [(Just wt1, cat --> [])] else do
    let m = seek "npb_w_dists" npb_w_dists pos
    case wordKey of
      Nothing ->
        catrule wordKey : rulesOfDist (fst m)
      Just (lexKey, posKey) ->
        case M.lookup (mod, side, key, posKey) (snd m) of
          Nothing -> [catrule Nothing]
          Just m ->
            case lexKey of
              Nothing ->
                catrule wordKey :
                (Just (fst (fst m)), catlex wordKey --> [catlex Nothing]) :
                rulesOfDist (snd (fst m))
              Just lexKey ->
                let wordKey' = Just (Nothing, posKey) in
                case M.lookup lexKey (snd m) of
                  Nothing -> [catrule wordKey']
                  Just m ->
                    catrule wordKey :
                    (Just (fst m), catlex wordKey --> [catlex wordKey']) :
                    rulesOfDist (snd m)

top_nt_counts :: Counts (Label, POS)
top_nt_counts = fst $ tally
  [ Obs () (unArg head, pos) count
  | Head (Word _ pos) (Just (parent, head)) count <- theEvents
  , parent == _TOP_ ]

top_nt_dist :: Dist (Label, POS)
top_nt_dist = rough top_nt_counts

top_w_counts :: Map POS (Counts Lex, Map Label (Counts Lex, ()))
top_w_counts = snd $ tally
  [ Obs (pos, case info of
                Just (parent, head) | parent == _TOP_ -> Just (unArg head, ())
                _ -> Nothing)
        lex count
  | Head (Word lex pos) info count <- theEvents ]

top_w_dists :: Map POS (Map Label (Dist Lex))
top_w_dists = M.map (uncurry (rough      >>> \d ->
              M.map (uncurry (smoothed d >>> \d -> \() -> d))))
            $ top_w_counts

head_counts :: Map Nont (Counts Label,
               Map POS  (Counts Label,
               Map Lex  (Counts Label, ())))
head_counts = snd $ tally
  [ Obs (unArg parent, (pos, (lex, ()))) (unArg head) count
  | Head (Word lex pos) (Just (parent, head)) count <- theEvents
  , parent /= _TOP_ ]

head_dists :: Map Nont (Dist Label,
              Map POS  (Dist Label,
              Map Lex  (Dist Label)))
head_dists = M.map (uncurry (rough      >>> \d -> ((,) d) .
             M.map (uncurry (smoothed d >>> \d -> ((,) d) .
             M.map (uncurry (smoothed d >>> \d -> \() -> d))))))
           $ head_counts

mod_nt_counts :: Map (Side, Nont, Label) (Counts (Label, POS),
                 Map POS                 (Counts (Label, POS),
                 Map Lex                 (Counts (Label, POS), ())))
mod_nt_counts = snd $ tally
  [ Obs ((side, unArg parent, unArg head), (posHead, (lexHead, ())))
        (unArg mod, pos)
        count
  | Modifier (Word _ pos) (Word lexHead posHead)
      mod _prev _ parent head _ side count <- theEvents
  , parent /= _TOP_
  , parent /= _NPB_ ]

mod_nt_dists :: Map (Side, Nont, Label) (Dist (Label, POS),
                Map POS                 (Dist (Label, POS),
                Map Lex                 (Dist (Label, POS))))
mod_nt_dists = M.map (uncurry (rough      >>> \d -> ((,) d) .
               M.map (uncurry (smoothed d >>> \d -> ((,) d) .
               M.map (uncurry (smoothed d >>> \d -> \() -> d))))))
             $ mod_nt_counts

mod_w_counts :: Map POS                             (Counts Lex,
                Map (Label, Side, Nont, Label, POS) (Counts Lex,
                Map Lex                             (Counts Lex, ())))
mod_w_counts = snd $ tally
  [ Obs (pos, if parent == _TOP_ || parent == _NPB_ then Nothing else Just
              ((unArg mod, side, unArg parent, unArg head, posHead),
               (lexHead, ())))
        lex
        count
  | Modifier (Word lex pos) (Word lexHead posHead)
      mod _prev _ parent head _ side count <- theEvents
  , pos /= _STOP_ ]

mod_w_dists :: Map POS                             (Dist Lex,
               Map (Label, Side, Nont, Label, POS) ((Prob, Dist Lex),
               Map Lex                             ((Prob, Dist Lex))))
mod_w_dists = M.map (rough  ***
              M.map (smooth ***
              M.map (smooth . \(d,())->d)))
            $ mod_w_counts

npb_nt_counts :: Map (Side, Label) (Counts (Label, POS),
                 Map POS           (Counts (Label, POS),
                 Map Lex           (Counts (Label, POS), ())))
npb_nt_counts = snd $ tally
  [ Obs ((side, unArg key), (posKey, (lexKey, ()))) (unArg mod, pos) count
  | Modifier (Word _ pos) (Word lexHead posHead)
      mod prev (Word lexPrev posPrev) parent head _ side count <- theEvents
  , parent == _NPB_
  , let (key, posKey, lexKey) | prev == _START_ = (head, posHead, lexHead)
                              | otherwise       = (prev, posPrev, lexPrev) ]

npb_nt_dists :: Map (Side, Label) (Dist (Label, POS),
                Map POS           (Dist (Label, POS),
                Map Lex           (Dist (Label, POS))))
npb_nt_dists = M.map (uncurry (rough      >>> \d -> ((,) d) .
               M.map (uncurry (smoothed d >>> \d -> ((,) d) .
               M.map (uncurry (smoothed d >>> \d -> \() -> d))))))
             $ npb_nt_counts

npbCat :: Side -> Label -> POS -> Lex -> Cat
npbCat side key posKey lexKey =
  NPB side key (let m = seek "npb_nt_dists" npb_nt_dists (side, key) in
                case M.lookup posKey (snd m) of
                  Nothing -> Nothing
                  Just m  -> case M.lookup lexKey (snd m) of
                               Nothing -> Just (Nothing    , posKey)
                               Just _m -> Just (Just lexKey, posKey))

npb_w_counts :: Map POS                       (Counts Lex,
                Map (Label, Side, Label, POS) (Counts Lex,
                Map Lex                       (Counts Lex, ())))
npb_w_counts = snd $ tally
  [ Obs (pos, if parent /= _NPB_ then Nothing else Just
              ((unArg mod, side, unArg key, posKey), (lexKey, ())))
        lex count
  | Modifier (Word lex pos) (Word lexHead posHead)
      mod prev (Word lexPrev posPrev) parent head _ side count <- theEvents
  , let (key, posKey, lexKey) | prev == _START_ = (head, posHead, lexHead)
                              | otherwise       = (prev, posPrev, lexPrev) ]

npb_w_dists :: Map POS                       (Dist Lex,
               Map (Label, Side, Label, POS) ((Prob, Dist Lex),
               Map Lex                       ((Prob, Dist Lex))))
npb_w_dists = M.map (rough  ***
              M.map (smooth ***
              M.map (smooth . \(d,())->d)))
            $ npb_w_counts

-- Debugging

class FoldableWithKey t where
  foldMapWithKey :: Monoid m => (((k0, k), a) -> m) -> (k0, t k a) -> m

instance FoldableWithKey Map where
  foldMapWithKey f (k0,m) = mconcat [ f ((k0,k),v) | (k,v) <- M.assocs m ]

instance FoldableWithKey (,) where
  foldMapWithKey f (k0,(k,v)) = f ((k0,k),v)

head_phw :: S.Set (Nont, Label, POS, Lex)
head_phw = (foldMapWithKey . foldMapWithKey . foldMapWithKey .
            foldMapWithKey . foldMapWithKey . foldMapWithKey)
           (\((((((((),parent),_),pos),_),lex),head),_) ->
            S.singleton (parent,head,pos,lex))
           ((),head_dists)

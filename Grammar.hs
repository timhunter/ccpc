{-# OPTIONS -W #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grammar (
    main, Grammar, mapReduce,
    theGrammar, theLexGrammar, theNonlexGrammar,
    theCats, theLexCats, theNonlexCats, theWords,
    theNumberedGrammar, Renum(..), theCFG, Cat(..),
    seek,
    top_nt_counts, top_nt_dist, top_w_counts, top_w_dists,
    head_counts, head_dists,
    mod_nt_counts, mod_nt_dists, modCat, mod_w_counts, mod_w_dists,
    npb_nt_counts, npb_nt_dists, npbCat, npb_w_counts, npb_w_dists
) where

import Prelude hiding (lex)
import Observed hiding (main)
import Prob (Counts, Prob, Dist, rough, smoothed, smooth, Obs(Obs), tally)
import MCFG
import Util (printListToFile, concurrently)
import CFG (CFG, Rule(Rule), RHS(RHS, prob, children, yield))
import qualified Reduce (mapReduce)
import Data.Monoid
import Data.Map (Map, (!))
import Data.Maybe (fromMaybe)
import Data.Bifunctor (first)
import Data.Binary (encodeFile)
import Control.Category ((>>>))
import Control.Arrow ((***))
import Control.DeepSeq (NFData(rnf))
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.Exception (evaluate)
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
  evaluate (rnf theEvents)
  evaluate (rnf theCats)
  concurrently $ printListToFile "wsj.cats" (M.keys (M.delete Start theCats))
               : printListToFile "wsj.words" (M.keys theWords)
               : encodeFile "wsj.cfg" theCFG
             : [ printListToFile ("wsj." ++ name) (thunk ())
               | (name, thunk) <- theNumberedGrammar ]

type Grammar cat term = [(String, () -> MCFG cat term)]

mapReduce :: (Monoid a, NFData a) =>
             (MRule cat term -> a) -> Grammar cat term -> a
mapReduce f grammar =
  mconcat (parMap rdeepseq (Reduce.mapReduce f . \x -> snd x ()) grammar)

theGrammar, theLexGrammar, theNonlexGrammar :: Grammar Cat Word
theGrammar       = theLexGrammar ++ theNonlexGrammar
theLexGrammar    = [ ("lex_rules" , lex_rules ) ]
theNonlexGrammar = [ ("top_rules" , top_rules )
                   , ("head_rules", head_rules)
                   , ("mod_rules" , mod_rules )
                   , ("npb_rules" , npb_rules ) ]

theLexCats :: M.Map Cat Word
theCats, theNonlexCats :: M.Map Cat ()
theCats = M.union (M.map (const ()) theLexCats) theNonlexCats
theLexCats = mapReduce f theLexGrammar
  where f (Rule lhs (RHS _ [] [[Term rhs]])) = M.singleton lhs rhs
        f _ = error "theLexCats encountered non-lexical rule"
theNonlexCats = mapReduce f theNonlexGrammar
  where f (Rule lhs _) = M.singleton lhs ()

theWords :: M.Map Word ()
theWords = M.fromList [ (Word lex pos, ())
                      | (lex, poss) <- M.toList thePosMap
                      , pos <- poss ]

newtype Renum = Renum Int deriving (Eq, Ord, NFData)

instance Show Renum where
  show (Renum 0) = "S"
  show (Renum i) = "t" ++ show i

theNumberedGrammar :: Grammar Renum Word
theNumberedGrammar =
  map (fmap (map (first (Renum . (`M.findIndex` theCats))) .)) theGrammar

theCFG :: CFG Double ()
theCFG = A.accumArray (flip (:)) [] (0, M.size theNonlexCats - 1)
           [ (M.findIndex lhs theNonlexCats,
              RHS { prob = fromRational wt, children = map f rhs, yield = () })
           | (_, thunk) <- theNonlexGrammar
           , rule <- thunk ()
           , let Rule lhs (RHS (Just wt) rhs _) = rule ]
  where f cat = case (M.lookupIndex cat theNonlexCats,
                      M.lookup cat theLexCats) of
                (Just nonlex, Nothing) -> nonlex
                (Nothing, Just word)   -> -1 - M.findIndex word theWords
                (Nothing, Nothing)     -> error ("theCFG: undefined " ++ s)
                (Just _, Just _)       -> error ("theCFG: redefined " ++ s)
          where s = show cat

data Prev = START | CC | PUNC | OTHER deriving (Eq, Ord, Show)

instance NFData Prev

isPunc :: Label -> Bool
isPunc l = l == _COMMA_ || l == _COLON_

prevOfLabel :: Label -> Prev
prevOfLabel prev | prev == _START_ = START
                 | prev == _CC_    = CC
                 | isPunc prev     = PUNC
                 | otherwise       = OTHER

type Lex_POS = Maybe (Maybe Lex, POS)
data Cat
  = Start
  | Init                        Label Lex POS
  | Bar                   Label Label Lex POS -- for binarized grammar only
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

instance Show Cat where
  show Start = "S"
  show (Init label lex pos) = "i" ++
    show label ++ "_" ++ show lex ++ show pos
  show (Bar parent head lexHead posHead) = "b" ++
    show head ++ "^" ++ show parent ++ "_" ++ show lexHead ++ show posHead
  show (Mod side parent head word) = showSide side ++
    show head ++ "^" ++ show parent ++ showHeadWord word
  show (ModLex side mod pos parent head word) = showSide side ++
    show mod ++ "_" ++ show pos ++ "_" ++
    show head ++ "^" ++ show parent ++ showHeadWord word
  show (NPB side key word) = showSide side ++
    show key ++ "^NPB" ++ showHeadWord word
  show (NPBLex side mod pos key word) = showSide side ++
    show mod ++ "_" ++ show pos ++ "_" ++
    show key ++ "^NPB" ++ showHeadWord word

showHeadWord :: Maybe (Maybe Lex, POS) -> String
showHeadWord Nothing                = ""
showHeadWord (Just (Nothing , pos)) = "_" ++ show pos
showHeadWord (Just (Just lex, pos)) = "_" ++ show lex ++ show pos

showSide :: Side -> String
showSide L = "l"
showSide R = "r"

infix 6 -->
(-->) :: (Rational, Cat) -> [Cat] -> MRule Cat Word
(wt, lhs) --> rhs = Rule lhs RHS{
  prob     = Just wt,
  children = rhs,
  yield    = [zipWith (\_ i -> Child i 0) rhs [0..]] }

cons :: Side -> a -> [a] -> [a]
cons L x xs = x : xs
cons R x xs = xs ++ [x]

seek :: (Ord k, Show k) => String -> Map k a -> k -> a
seek tag m k = fromMaybe (error ("seek (" ++ tag ++ ") " ++ show k))
                         (M.lookup k m)

possibleHeadPos :: Label -> POS -> Bool
-- If a head label is a POS tag or a nonterminal label that can only generate
-- a POS tag in turn, then this label must match the head word's POS.  In
-- this sense, Collins and Bikel's generative models are deficient.
possibleHeadPos head pos =
  if S.member head thePOSs
  then head == pos
  else maybe True (S.member pos) (M.lookup head antepreterminals)

top_rules :: () -> MCFG Cat Word
top_rules () = do ((nt, pos), wt1) <- M.toList top_nt_dist
                  (lex      , wt2) <- M.toList (top_w_dists ! pos ! nt)
                  [(wt1 * wt2, Start) --> [Init nt lex pos]]

lex_rules :: () -> MCFG Cat Word
lex_rules () = do (lex, poss) <- M.toList thePosMap
                  pos <- poss
                  [Rule (Init pos lex pos) RHS{
                    prob     = Just 1,
                    children = [],
                    yield    = [[Term (Word lex pos)]] }]

head_rules :: () -> MCFG Cat Word
head_rules () = do
  (parent, m) <- M.toList head_dists
  (lex, poss) <- M.toList thePosMap
  pos <- poss
  let dist = maybe (fst m)
                   (\m -> maybe (fst m)
                                (\m -> m)
                                (M.lookup lex (snd m)))
                   (M.lookup pos (snd m))
      (distPossible, distImpossible) =
        M.partitionWithKey (\head _prob -> possibleHeadPos head pos) dist
      dist' = {- M.map (/ (1 - sum (M.elems distImpossible))) -} distPossible
  (head, wt) <- M.toList dist'
  let cat | parent == _NPB_ = \side -> npbCat side head pos lex
          | otherwise       = \side -> modCat side parent head pos lex
  [(wt, Init parent lex pos) --> [cat L, {- Bar parent head lex pos],
   (1 , Bar parent head lex pos) --> [-} Init head lex pos, cat R]]

mod_rules :: () -> MCFG Cat Word
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
      catrule wordHead = (wt1, cat) --> cons side cat [catlex wordHead]
      rulesOfDist dist = [ (wt2, catlex wordHead) --> [Init mod lex pos]
                         | (lex, wt2) <- M.toList dist ]
  if mod == _STOP_ then [(wt1, cat) --> []] else do
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
                (fst (fst m), catlex wordHead) --> [catlex Nothing] :
                rulesOfDist (snd (fst m))
              Just lexHead ->
                let wordHead' = Just (Nothing, posHead) in
                case M.lookup lexHead (snd m) of
                  Nothing -> [catrule wordHead']
                  Just m ->
                    catrule wordHead :
                    (fst m, catlex wordHead) --> [catlex wordHead'] :
                    rulesOfDist (snd m)

npb_rules :: () -> MCFG Cat Word
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
      catrule wordKey  = (wt1, cat) --> [catlex wordKey]
      rulesOfDist dist = [ (wt2, catlex wordKey)
                           --> cons side (if isPunc mod then {- generating punctuation leaves the "previous" ("key") nonterminal/POS/word unchanged -} catlex wordKey {- slightly incorrect when the grammar generates (catlex wordKey) due to backoff from another Lex_POS -} else npbCat side mod pos lex)
                                         [Init mod lex pos]
                         | (lex, wt2) <- M.toList dist ]
  if mod == _STOP_ then [(wt1, cat) --> []] else do
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
                (fst (fst m), catlex wordKey) --> [catlex Nothing] :
                rulesOfDist (snd (fst m))
              Just lexKey ->
                let wordKey' = Just (Nothing, posKey) in
                case M.lookup lexKey (snd m) of
                  Nothing -> [catrule wordKey']
                  Just m ->
                    catrule wordKey :
                    (fst m, catlex wordKey) --> [catlex wordKey'] :
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

modCat :: Side -> Nont -> Label -> POS -> Lex -> Cat
modCat side parent head posHead lexHead = Mod side parent head $
  let m = seek "mod_nt_dists" mod_nt_dists (side, parent, head) in
  case M.lookup posHead (snd m) of
    Nothing -> Nothing
    Just m  -> case M.lookup lexHead (snd m) of
                 Nothing -> Just (Nothing     , posHead)
                 Just _m -> Just (Just lexHead, posHead)

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
npbCat side key posKey lexKey = NPB side key $
  let m = seek "npb_nt_dists" npb_nt_dists (side, key) in
  case M.lookup posKey (snd m) of
    Nothing -> Nothing
    Just m  -> case M.lookup lexKey (snd m) of
                 Nothing -> Just (Nothing    , posKey)
                 Just _m -> Just (Just lexKey, posKey)

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

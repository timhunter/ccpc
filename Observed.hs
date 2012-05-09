{-# OPTIONS -W #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Observed (
    main,
    Sexp(..), sexp, theTrees, theTrees',
    Lex(..), Label(..), Nont, POS, Word(..), Side(..), Event(..),
    _TOP_, _START_, _STOP_, _NPB_, _CC_, _COMMA_, _COLON_, _UNKNOWN_,
    event, theEvents, theNonts, unArg, reArg, thePOSs, thePosMap, known,
    antepreterminals
) where

import Prob (Count)
import Data.Atom.Simple
import Control.DeepSeq (deepseq, NFData(rnf))
import Data.Word (Word8)
import Codec.Compression.GZip (decompress)
import Control.Monad.State
import Data.Maybe (catMaybes)
import GHC.Exts (IsString(fromString)) -- for -XOverloadedStrings
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Char

main :: IO ()
main = theEvents `deepseq` print thePosMap

newtype Lex   = Lex   Symbol deriving (Eq, Ord, NFData)
newtype Label = Label Symbol deriving (Eq, Ord, NFData)
type    Nont  = Label
type    POS   = Label

instance Show Lex   where show (Lex   sym) = extern sym ++ "/"
instance Show Label where show (Label sym) = extern sym

instance IsString Lex   where fromString = Lex   . intern
instance IsString Label where fromString = Label . intern

_TOP_, _START_, _STOP_, _NPB_, _CC_, _COMMA_, _COLON_ :: Label
_TOP_   = Label (intern "+TOP+")
_START_ = Label (intern "+START+")
_STOP_  = Label (intern "+STOP+")
_NPB_   = Label (intern "NPB")
_CC_    = Label (intern "CC")
_COMMA_ = Label (intern ",")
_COLON_ = Label (intern ":")

_UNKNOWN_ :: Lex
_UNKNOWN_ = Lex (intern "+unknown+")

data Word = Word !Lex !Label deriving (Eq, Ord)
data Side = L | R            deriving (Eq, Ord, Show)

instance Show Word where show (Word lex pos) = show lex ++ show pos
instance Read Word where
  readsPrec _ = readParen False (\r ->
    let (word, rest) = break Data.Char.isSpace (dropWhile Data.Char.isSpace r)
        (rev_pos, rev_lex) = span ('/'/=) (reverse word)
    in case rev_lex of
       '/':rev_lex -> [(Word (Lex   (intern (reverse rev_lex)))
                             (Label (intern (reverse rev_pos))), rest)]
       _ -> [])

instance NFData Symbol
instance NFData Word where rnf (Word lex pos) = rnf lex `seq` rnf pos
instance NFData Side

data Event
  = Nonterminal !Label {-# UNPACK #-} !Count
  | Head {-# UNPACK #-} !Word !(Maybe (Nont, Label)) {-# UNPACK #-} !Count
  | Modifier {-# UNPACK #-} !Word {-# UNPACK #-} !Word !Label !Label
      {-# UNPACK #-} !Word !Nont !Label !Bool !Side {-# UNPACK #-} !Count
  deriving (Eq, Ord, Show)

instance NFData Event where
  rnf (Nonterminal nt count) = rnf nt `seq` rnf count
  rnf (Head word info count) = rnf word `seq` rnf info `seq` rnf count
  rnf (Modifier wordMod wordHead mod prev wordPrev parent head vi side count)
    = rnf wordMod `seq` rnf wordHead `seq` rnf mod `seq` rnf prev `seq`
      rnf wordPrev `seq` rnf parent `seq` rnf head `seq` rnf vi `seq`
      rnf side `seq` rnf count

type Parser = State B.ByteString

isSpace :: Word8 -> Bool
isSpace c = c == 32 || c == 10 || c == 13 || c == 9

isBreak :: Word8 -> Bool
isBreak c = isSpace c || c == 40 || c == 41

token :: Parser String
token = state (\s ->
  case B.uncons s of
    Nothing      -> ([], s)
    Just (40, s) -> ("(", B.dropWhile isSpace s)
    Just (41, s) -> (")", B.dropWhile isSpace s)
    Just (c , s) -> case B.break isBreak s of { (rest, s) ->
                    (C.unpack (B.cons c rest), B.dropWhile isSpace s) })

sym :: Parser Symbol
sym = do x <- token
         if x == "(" || x == ")"
           then error ("Unexpected `" ++ x ++ "'")
           else return $! intern x

count :: Parser Count
count = state (\s ->
  case C.readInt s of
    Nothing    -> error "Integer expected"
    Just (i,s) -> (i, B.dropWhile isSpace
                     (B.dropWhile (\c -> c == 46 || c == 48) s)))

parens :: Parser a -> Parser a
parens m = do "(" <- token
              a <- m
              ")" <- token
              return a

many :: Parser a -> Parser [a]
many m = loop where
  loop = do done <- gets (\s -> B.null s || 41 == B.head s)
            if done then return [] else liftM2 (:) m loop

data Sexp = Atom !Symbol | List [Sexp] deriving (Eq, Ord)
instance Show Sexp where
  showsPrec _ (Atom sym)    = showString (extern sym)
  showsPrec _ (List [])     = showString "()"
  showsPrec _ (List (e:es)) = showChar '(' . showsPrec 0 e . s es . showChar ')'
    where s []     = id
          s (e:es) = showChar ' ' . showsPrec 0 e . s es
sexp :: Parser Sexp
sexp = do x <- token
          case x of
            "(" -> do es <- many sexp
                      ")" <- token
                      return (List es)
            ")" -> error "Unmatched `)'"
            _   -> return $! Atom (intern x)

word :: Parser Word
word = do "(" <- token
          lex <- sym
          pos <- sym
          next <- token
          case next of
            ")"         -> do return (Word (Lex lex) (Label pos))
            "+unknown+" -> do ")" <- token
                              return (Word _UNKNOWN_ (Label pos))
            _           -> error ("Unknown word property: " ++ next)

event :: Parser (Maybe Event)
event = do "(" <- token
           tag <- token
           case tag of
             "nonterminal" -> do
               nt <- sym
               n <- count
               return (Just (Nonterminal (Label nt) n))
             "head" -> do
               "(" <- token
               w@(Word _ (Label pos)) <- word
               parent <- sym
               head <- sym
               parens (many token)
               parens (many token)
               ")" <- token
               n <- count
               let info | pos == parent = Nothing
                        | otherwise     = Just (Label parent, Label head)
               return (Just (Head w info n))
             "mod" -> do
               "(" <- token
               wordMod <- word
               wordHead <- word
               mod <- sym
               prev <- parens sym
               wordPrev <- parens word
               parent <- sym
               head <- sym
               parens (many token)
               vi <- token
               side <- token
               ")" <- token
               n <- count
               return (Just (Modifier wordMod wordHead (Label mod)
                              (Label prev) wordPrev (Label parent) (Label head)
                              (case vi of "true" -> True
                                          "false" -> False
                                          _ -> error ("Unknown vi: " ++ vi))
                              (case side of "left" -> L
                                            "right" -> R
                                            _ -> error ("Unknown side: " ++ side))
                              n))
             _ -> return Nothing

readGzipFile :: FilePath -> B.ByteString
readGzipFile fname = decompress (unsafePerformIO (C.readFile fname))

theTrees :: [Sexp]
theTrees = evalState (many (parens sexp))
                     (B.dropWhile isSpace (readGzipFile "wsj-02-21.mrg.gz"))

theTrees' :: [Sexp]
theTrees' = evalState (many (parens sexp))
                      (B.dropWhile isSpace (readGzipFile "wsj-22.mrg.gz"))

theEvents :: [Event]
theEvents = catMaybes (map (evalState event) (C.lines (readGzipFile fname)))
  where fname = "wsj-02-21.observed.gz" -- "observed-sampled.gz"

theNonts :: [Label]
theNonts = [ nt | Nonterminal nt _ <- theEvents ]

unArg :: Label -> Label
unArg = \nt -> case M.lookup nt table of
                 Just nt' -> nt'
                 Nothing  -> error ("unArg " ++ show nt)
  where table = M.fromList unArgList

reArg :: Label -> [Label]
reArg = \nt -> case M.lookup nt table of
                 Just nt' -> nt'
                 Nothing  -> error ("reArg " ++ show nt)
  where table = M.fromListWith (++) [ (nt, [ntA]) | (ntA, nt) <- unArgList ]

unArgList :: [(Label, Label)]
unArgList = [ (ntA, case reverse (extern sym) of
                      'A':'-':tn -> Label (intern (reverse tn))
                      _          -> ntA)
            | ntA@(Label sym) <-
                _TOP_ : _STOP_ : theNonts ++ S.toList thePOSs ]

thePOSs :: S.Set POS
thePOSs = S.fromList [ pos | Head (Word _ pos) Nothing _ <- theEvents ]

thePosMap :: M.Map Lex [Label]
thePosMap = M.map S.toList $
            M.fromListWith S.union
              [ (lex, S.singleton pos)
              | Head (Word lex pos) Nothing _ <- theEvents ]

known :: Word -> Bool
known (Word lex pos) = maybe False (pos `elem`) (M.lookup lex thePosMap)

antepreterminals :: M.Map Nont (S.Set POS)
-- If a nonterminal appears in this map, it means that it is compatible
-- with only certain head-word POS-tags. In other words, it can reach only
-- the listed POS-tags by the observed parent--head-child relation.
antepreterminals =
  M.mapMaybe (\heads -> if heads `S.isSubsetOf` thePOSs
                        then Just heads else Nothing)
             (M.fromListWith S.union
                [ (unArg parent, S.singleton (unArg head))
                | Head _ (Just (parent, head)) _ <- theEvents ])

extern :: Symbol -> String
extern = tail . dropWhile ('>' /=) . show

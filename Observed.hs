module Observed (
    Lex(..), Label(..), Nont, POS, Count,
    Unknown(..), Word(..), Subcat(..), Side(..), Event(..),
    event, parseFromGZipFile, theEvents, _TOP_, theNonts, unArg, reArg, thePOSs, thePosMap
) where

import Data.List (union, sort)
import Data.Atom.Simple
import Control.Applicative ((<*>))
import Control.Monad (guard)
import Data.Char (isPrint, isSpace)
import Text.Parsec hiding (count)
import Text.Parsec.ByteString.Lazy (Parser)
import Codec.Compression.GZip (decompress)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Text.Parsec.Token as P
import qualified Data.Set as S
import qualified Data.Map as M

P.TokenParser {
    P.identifier = identifier,
    P.symbol     = symbol,
    P.decimal    = decimal,
    P.parens     = parens,
    P.whiteSpace = whiteSpace
} = P.makeTokenParser P.LanguageDef {
    P.commentStart    = "",
    P.commentEnd      = "",
    P.commentLine     = "",
    P.nestedComments  = False,
    P.identStart      = ident,
    P.identLetter     = ident,
    P.opStart         = parserZero,
    P.opLetter        = parserZero,
    P.reservedNames   = [],
    P.reservedOpNames = [],
    P.caseSensitive   = True
} where ident = satisfy (\c -> isPrint c && not (isSpace c || elem c "()"))

list :: Parser a -> Parser [a]
list p = parens (many p)

count :: Parser Count
count = decimal >>= \n -> symbol ".0" >> return n

sym :: Parser Symbol
sym = identifier >>= \x -> return $! intern x

newtype Lex   = Lex   Symbol deriving (Eq, Ord)
newtype Label = Label Symbol deriving (Eq, Ord)
type    Nont  = Label
type    POS   = Label
type    Count = Integer

instance Show Lex   where show (Lex   sym) = extern sym ++ "/"
instance Show Label where show (Label sym) = extern sym

data    Unknown = Unknown                        deriving (Eq, Ord, Show)
data    Word    = Word Lex Label (Maybe Unknown) deriving (Eq, Ord, Show)
newtype Subcat  = Subcat [Label]                 deriving (Eq, Ord, Show)
data    Side    = L | R                          deriving (Eq, Ord, Show)

data Event = Nonterminal Label Count
           | Head Word (Maybe (Nont, Label, Subcat, Subcat)) Count
           | Modifier Word Word Label [Label] [Word] Nont Label Subcat Bool Side Count
           | Vocab Lex Count
           | WordFeature Unknown Count
           | Pos Lex [POS]
           | PrunedPreterm [(Symbol, Symbol)]
           | PrunedPunc [(Symbol, Symbol)]
  deriving (Eq, Ord, Show)

unknown :: Parser Unknown
unknown = symbol "+unknown+" >> return Unknown

word :: Parser Word
word = parens (return Word <*> fmap Lex sym <*> fmap Label sym <*> optionMaybe unknown)

subcat :: Parser Subcat
subcat = fmap (Subcat . sort) (list (fmap Label sym))

bool :: Parser Bool
bool = (symbol "true"  >> return True ) <|>
       (symbol "false" >> return False)

side :: Parser Side
side = (symbol "left"  >> return L) <|>
       (symbol "right" >> return R)

pair :: Parser (Symbol, Symbol)
pair = parens (return (,) <*> sym <*> sym)

event :: Parser Event
event = parens (identifier >>= dispatch) where
  dispatch "nonterminal"    = return (Nonterminal . Label) <*> sym <*> count
  dispatch "head"           = parens (do
                                w@(Word (Lex lex) (Label pos) _) <- word
                                parent <- sym
                                head   <- sym
                                if pos == parent
                                  then do guard (lex == head)
                                          parens (return ())
                                          parens (return ())
                                          return (Head w Nothing)
                                  else do left  <- subcat
                                          right <- subcat
                                          return (Head w (Just (Label parent, Label head, left, right))))
                              <*> count
  dispatch "mod"            = parens (return Modifier
                                      <*> word <*> word <*> fmap Label sym
                                      <*> list (fmap Label sym) <*> list word
                                      <*> fmap Label sym <*> fmap Label sym
                                      <*> subcat <*> bool <*> side)
                              <*> count
  dispatch "vocab"          = return Vocab <*> fmap Lex sym <*> count
  dispatch "word-feature"   = return WordFeature <*> unknown <*> count
  dispatch "pos"            = return Pos <*> fmap Lex sym <*> list (fmap Label sym)
  dispatch "pruned-preterm" = return PrunedPreterm <*> list pair
  dispatch "pruned-punc"    = return PrunedPunc <*> list pair
  dispatch _                = parserZero

parseFromGZipFile :: Parser a -> SourceName -> IO (Either ParseError a)
parseFromGZipFile p fname = fmap (runP p () fname . decompress)
                                 (C.readFile fname)

theEvents :: [Event]
theEvents = unsafePerformIO $ do
  Right r <- parseFromGZipFile (many event)
               "/home/ccshan/lib/parsers/bikel-parser/wsj-02-21.observed.gz"
               -- "/home/ccshan/lib/parsers/bikel-parser/observed-sampled.gz"
  return r

_TOP_ :: Nont
_TOP_ = Label (intern "+TOP+")

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
                 Nothing  -> error ("unArg " ++ show nt)
  where table = M.fromListWith (++) [ (nt, [ntA]) | (ntA, nt) <- unArgList ]

unArgList :: [(Label, Label)]
unArgList = [ (ntA, case reverse (extern sym) of
                      'A':'-':tn -> Label (intern (reverse tn))
                      _          -> ntA)
            | ntA@(Label sym) <- _TOP_ : theNonts ++ thePOSs ]

thePOSs :: [Label]
thePOSs = S.toList (S.unions [ S.fromList poss | Pos _ poss <- theEvents ])

thePosMap :: M.Map Lex [Label]
thePosMap = M.fromListWith union [ (lex,poss) | Pos lex poss <- theEvents ]

extern :: Symbol -> String
extern = tail . dropWhile ('>' /=) . show

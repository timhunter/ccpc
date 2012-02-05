{-# OPTIONS -W #-}

module MCFGRead (mcfgFromFile) where

import CFG
import MCFG
import Data.Maybe (catMaybes)
import Control.Monad (liftM, liftM2)
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec
import Text.Parsec.ByteString (Parser, parseFromFile)
import qualified Text.Parsec.Token as P

P.TokenParser {
    P.identifier     = identifier,
    P.symbol         = symbol,
    P.decimal        = decimal,
    P.naturalOrFloat = naturalOrFloat,
    P.brackets       = brackets,
    P.semiSep1       = semiSep1,
    P.comma          = comma,
    P.whiteSpace     = whiteSpace
} = P.makeTokenParser P.LanguageDef {
    P.commentStart    = "(*",
    P.commentEnd      = "*)",
    P.commentLine     = "#",
    P.nestedComments  = False,
    P.identStart      = letter   <|> oneOf "_-",
    P.identLetter     = alphaNum <|> oneOf "_-",
    P.opStart         = parserZero,
    P.opLetter        = parserZero,
    P.reservedNames   = [],
    P.reservedOpNames = [],
    P.caseSensitive   = True
}

quote :: Parser String
quote = symbol "\""

around :: Parser b -> (a -> c -> d) -> Parser a -> Parser c -> Parser d
around delim f p q = do x <- p
                        delim
                        y <- q
                        return (f x y)

mcfgFromFile :: String -> MCFG String String
mcfgFromFile name = unsafePerformIO (do
    result <- parseFromFile (between whiteSpace eof mcfg) name
    either (fail . show) return result)

mcfg :: Parser (MCFG String String)
mcfg = many (liftM2 f (optionMaybe weight) rule)
  where f wt (Rule lhs rhs) = Rule lhs rhs{prob=wt}

weight :: Parser Rational
weight = chainl1 (liftM (either toRational toRational) naturalOrFloat)
                 (symbol "/" >> return (/))

rule :: Parser (Rule () String [[Part String]])
rule = around (symbol "-->") Rule identifier rhs

rhs :: Parser (RHS () String [[Part String]])
rhs = liftM2 (RHS ())
             (many identifier)
             (    many1 (liftM catMaybes (brackets (semiSep1 part)))
              <|> between quote quote
                    (liftM (:[]) (many (liftM Term identifier))) )

part :: Parser (Maybe (Part String))
part = (symbol "Epsilon" >> return Nothing)
   <|> (around comma
          (\i j -> Just (Child (fromIntegral i) (fromIntegral j)))
          decimal
          decimal)
   <|> (between quote quote (liftM (Just . Term) identifier))

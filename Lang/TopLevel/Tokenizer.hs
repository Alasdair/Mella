{-# LANGUAGE OverloadedStrings #-}

module Lang.TopLevel.Tokenizer (Token (..), tokenize) where

import Control.Applicative
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as Atto hiding (skipSpace)
import Data.Attoparsec.Combinator as Atto

import Lang.Term.Tokenizer (ident)

data Token = TokFun
           | TokTheorem
           | TokRefl
           | TokColon
           | TokPeriod
           | TokIdent Text
           | TokTerm Text
           | TokQuote Text
           | TokOpen
           | TokClose
           | TokKeyword Text
           deriving (Show)

lineComment :: Parser ()
lineComment = do
    string "--"
    skipWhile (/= '\n')

blockComment :: Parser ()
blockComment = do
    string "{-" >> manyTill anyChar (try (string "-}")) >> return ()

skipSpace :: Parser ()
skipSpace = skipMany $ choice [space >> return (), lineComment, blockComment]

rest :: Parser Text
rest = takeTill (== '.')

fun :: Parser Token
fun = string "fun" >> return TokFun

theorem :: Parser Token
theorem = string "theorem" >> return TokTheorem

refl :: Parser Token
refl = string "refl" >> return TokRefl

colon :: Parser Token
colon = char ':' >> return TokColon

period :: Parser Token
period = char '.' >> return TokPeriod

term :: Parser Token
term = do
    char '"'
    t <- takeTill (== '"')
    char '"'
    return (TokTerm t)

quote :: Parser Token
quote = do
    char '\''
    t <- takeTill (== '\'')
    char '\''
    return (TokQuote t)

open :: Parser Token
open = char '(' >> return TokOpen

close :: Parser Token
close = char ')' >> return TokClose

keyword = TokKeyword <$> (char ':' >> ident)

token :: Parser Token
token = fun
        <|> theorem
        <|> refl
        <|> keyword
        <|> colon
        <|> period
        <|> open
        <|> close
        <|> term
        <|> quote
        <|> TokIdent <$> ident

tokenize' :: Parser [Token]
tokenize' = do
    toks <- many (skipSpace >> token)
    skipSpace
    endOfInput
    return toks

tokenize :: Text -> Maybe [Token]
tokenize t = case flip feed "" (Atto.parse tokenize' t) of
               (Done _ t') -> Just t'
               _ -> Nothing

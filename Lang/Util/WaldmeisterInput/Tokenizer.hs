{-# LANGUAGE OverloadedStrings #-}
module Lang.Util.WaldmeisterInput.Tokenizer where

import Control.Applicative
import Control.Monad

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Attoparsec.Text as Atto hiding (skipSpace)
import qualified Data.Attoparsec.Text as Atto (skipSpace)
import Data.Attoparsec.Combinator as Atto

import Lang.Error
import Lang.PrettyPrint

data Token = TokName Text
           | TokMode Text
           | TokIdentifier Text
           | TokSorts
           | TokColon
           | TokArrow
           | TokSignature
           | TokOrdering
           | TokLPO
           | TokKBO
           | TokGT
           | TokVariables
           | TokComma
           | TokEquations
           | TokEq
           | TokOpenBracket
           | TokCloseBracket
           | TokConclusion
           | TokInt Int
           deriving (Show)

comment :: Parser ()
comment = do
  string "%"
  skipWhile (/= '\n')

skipSpace :: Parser ()
skipSpace = skipMany ((space >> return ()) <|> comment)

openBracket :: Parser Token
openBracket = char '(' >> return TokOpenBracket

closeBracket :: Parser Token
closeBracket = char ')' >> return TokCloseBracket

name :: Parser Token
name = do
  string "NAME"
  skipSpace
  TokName <$> takeTill isSpace

mode :: Parser Token
mode = do
  string "MODE"
  skipSpace
  TokMode <$> takeTill isSpace

reservedWords = [ "KBO", "LPO", "SIGNATURE", "MODE", "PROOF", "NAME"
                , "EQUATIONS", "VARIABLES", "CONCLUSION", "ORDERING" 
                , "SORTS" ]

identifier :: Parser Token
identifier = do
  t <- Atto.takeWhile1 (\c -> isAlphaNum c || c `elem` ['_'])
  when (elem t reservedWords) $ fail (show t ++ " is a reserved word")
  return (TokIdentifier t)

sorts :: Parser Token
sorts = string "SORTS" >> return TokSorts

colon :: Parser Token
colon = char ':' >> return TokColon

gt :: Parser Token
gt = char '>' >> return TokGT

eq :: Parser Token
eq = char '=' >> return TokEq

conclusion :: Parser Token
conclusion = string "CONCLUSION" >> return TokConclusion

kbo :: Parser Token
kbo = string "KBO" >> return TokKBO

lpo :: Parser Token
lpo = string "LPO" >> return TokLPO

equations :: Parser Token
equations = string "EQUATIONS" >> return TokEquations

comma :: Parser Token
comma = string "," >> return TokComma

ordering :: Parser Token
ordering = string "ORDERING" >> return TokOrdering

arrow :: Parser Token
arrow = string "->" >> return TokArrow

signature :: Parser Token
signature = string "SIGNATURE" >> return TokSignature

variables :: Parser Token
variables = string "VARIABLES" >> return TokVariables

int :: Parser Token
int = TokInt . read <$> many1 digit

token :: Parser Token
token = name
        <|> mode
        <|> sorts
        <|> colon
        <|> arrow
        <|> signature
        <|> ordering
        <|> lpo
        <|> kbo
        <|> gt
        <|> variables
        <|> comma
        <|> equations
        <|> eq
        <|> openBracket
        <|> closeBracket
        <|> conclusion
        <|> int
        <|> identifier

tokenize' :: Parser [Token]
tokenize' = do
    toks <- Atto.many (skipSpace >> token)
    skipSpace
    endOfInput
    return toks

data TokenizerError = TErr Text Text

instance ErrorMsg TokenizerError where
    toPrettyText (TErr remaining err) sch =
        T.concat [ colorBanner sch errorColor "PARSE ERROR"
                 , "\nTokenizer failed with error:\n"
                 , err
                 , "\nRemaining input:\n"
                 , remaining
                 ]

tokenize :: Text -> Error [Token]
tokenize t =
    case flip feed "" $ Atto.parse tokenize' t of
      (Done _ t') -> return t'
      (Fail remaining _ err) -> throwError (TErr remaining (T.pack err))
      (Partial _) -> throwError (TErr "Tokenizer demanding more input." "N/A")

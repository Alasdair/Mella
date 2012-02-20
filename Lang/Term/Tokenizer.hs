{-# LANGUAGE OverloadedStrings #-}

module Lang.Term.Tokenizer where

import Control.Applicative
import Control.Monad

import Data.Char
import Data.List (foldl')
import Data.Word

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as Atto
import Data.Attoparsec.Combinator as Atto

import Lang.Term
import Lang.Error
import Lang.PrettyPrint

data Token = TokArrow
           | TokLambda
           | TokOpenBracket
           | TokCloseBracket
           | TokIdentifier Text
           | TokHasType
           | TokId
           | TokJ
           | TokRefl
           | TokNat
           | TokNumeric Int
           | TokSort Sort
           | TokAnnotate
           | TokMeta
           deriving (Show)

int :: Parser Int
int = do
    d <- map digitToInt <$> many1 (satisfy isDigit)
    return $ foldl' (\n m -> n * 10 + m) 0 d

word :: Parser Word
word = do
    d <- map (read . pure) <$> many1 (satisfy isDigit)
    return $ foldl' (\n m -> n * 10 + m) 0 d

whitespace = [' ', '\t', '\n']

arrow :: Parser Token
arrow = (string "→" <|> string "->") >> return TokArrow

lambda :: Parser Token
lambda = (char 'λ' <|> char '\\') >> return TokLambda

reservedWords = [ "\\", "λ", "Box", "□", "*", "."
                , "->", "→", "ℕ", "Nat", "?", "refl"
                , "suc", "zero", "rewrite", "ltr"
                , "rtl", "Id", ":", "::", "elimJ" ]

illegalChars :: [Char]
illegalChars = ['(', ')', '"', '.', ';', '{', '}']

ident :: Parser Text
ident = do
    t <- Atto.takeWhile1 (flip notElem (whitespace ++ illegalChars))
    when (elem t reservedWords) $ fail (show t ++ " is a reserved word")
    return t

identifier :: Parser Token
identifier = TokIdentifier <$> ident

openBracket :: Parser Token
openBracket = char '(' >> return TokOpenBracket

closeBracket :: Parser Token
closeBracket = char ')' >> return TokCloseBracket

hasType :: Parser Token
hasType = char ':' >> return TokHasType

identity :: Parser Token
identity = string "Id" >> return TokId

axiomJ :: Parser Token
axiomJ = string "elimJ" >> return TokJ

refl :: Parser Token
refl = string "refl" >> return TokRefl

nat :: Parser Token
nat = (string "Nat" <|> string "ℕ") >> return TokNat

numeric :: Parser Token
numeric = int >>= return . TokNumeric

box :: Parser Token
box = do
    (string "□" <|> string "Box")
    skipSpace
    w <- word
    return (TokSort (Box w))

star :: Parser Token
star = char '*' >> return (TokSort Star)

annotate :: Parser Token
annotate = string "::" >> return TokAnnotate

meta :: Parser Token
meta = char '?' >> return TokMeta

token :: Parser Token
token = arrow
        <|> lambda
        <|> axiomJ
        <|> openBracket
        <|> closeBracket
        <|> identifier
        <|> annotate
        <|> hasType
        <|> identity
        <|> refl
        <|> nat
        <|> numeric
        <|> box
        <|> star
        <|> meta

tokenize' :: Parser [Token]
tokenize' = do
    toks <- many (skipSpace >> token)
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

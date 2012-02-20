{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Lang.Term.Tokenizer
    ( Token (..)
    , tokenize
    ) where

import Control.Applicative

import qualified Data.Char as Char
import Data.List (foldl')
import Data.Word

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as Atto
import Data.Attoparsec.Combinator as Atto

import Lang.Term
import Lang.Term.Identifier
import Lang.Term.Operator
import Lang.Error
import Lang.PrettyPrint

data Token = TokArrow
           | TokLambda
           | TokOpenBracket
           | TokCloseBracket
           | TokOpenType
           | TokCloseType
           | TokIdentifier { unIdent :: Text }
           | TokHasType
           | TokId
           | TokJ
           | TokRefl
           | TokSortBox { unBox :: Word }
           | TokSortStar
           | TokAnnotate
           | TokMeta
           | TokNamePart { unNamePart :: Text }
           deriving (Show, Eq, Ord)

arrow :: Parser Token
arrow = (string "→" <|> string "->") >> return TokArrow

lambda :: Parser Token
lambda = (char 'λ' <|> char '\\') >> return TokLambda

identifier :: [Operator] -> Parser Token
identifier ops = do
    i <- ident
    return $ if i `elem` (nameParts =<< ops)
             then TokNamePart i
             else TokIdentifier i

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

word :: Parser Word
word = do
    d <- map (read . pure) <$> many1 (satisfy Char.isDigit)
    return $ foldl' (\n m -> n * 10 + m) 0 d

box :: Parser Token
box = do
    (string "□" <|> string "Box")
    TokSortBox <$> word

star :: Parser Token
star = char '*' >> return TokSortStar

annotate :: Parser Token
annotate = string "::" >> return TokAnnotate

meta :: Parser Token
meta = char '?' >> return TokMeta

token :: [Operator] -> Parser Token
token ops = arrow
        <|> lambda
        <|> axiomJ
        <|> openBracket
        <|> closeBracket
        <|> box
        <|> identifier ops
        <|> annotate
        <|> hasType
        <|> identity
        <|> refl
        <|> star
        <|> meta

tokenize' :: [Operator] -> Parser [Token]
tokenize' ops = do
    toks <- many (skipSpace >> token ops)
    skipSpace >> endOfInput
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

tokenize :: [Operator] -> Text -> Error [Token]
tokenize ops t =
    case flip feed "" $ Atto.parse (tokenize' ops) t of
      (Done _ t') -> return (fixBrackets [] t')
      (Fail remaining _ err) -> throwError (TErr remaining (T.pack err))
      (Partial _) -> throwError (TErr "Tokenizer demanding more input." "N/A")


fixBrackets :: [Char] -> [Token] -> [Token]
fixBrackets stack tokens = case (stack, tokens) of
    (_, TokOpenBracket : toks) -> if isTypeBracket toks
                                  then TokOpenType : fixBrackets ('{' : stack) toks
                                  else TokOpenBracket : fixBrackets ('(' : stack) toks
    ('(' : s, TokCloseBracket : toks) -> TokCloseBracket : fixBrackets s toks
    ('{' : s, TokCloseBracket : toks) -> TokCloseType : fixBrackets s toks
    (_, t : toks) -> t : fixBrackets stack toks
    (_, []) -> []
  where
    isTypeBracket ((TokIdentifier _) : toks) = isTypeBracket toks
    isTypeBracket (TokHasType : toks) = True
    isTypeBracket _ = False

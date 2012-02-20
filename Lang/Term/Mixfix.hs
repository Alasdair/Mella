{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Lang.Term.Mixfix
    ( Token (..)
    , tokenize
    , Fixity (..)
    , Precedence (..)
    , Operator, opName
    , fixity, precedence, arity, nameParts
    , mkOp
    , isValidOp
    ) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad

import qualified Data.Char as Char
import Data.List (foldl', intersperse)
import qualified Data.Maybe as Maybe
import qualified Data.Data as D
import Data.Word

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as Atto
import Data.Attoparsec.Combinator as Atto

import Lang.Term
import Lang.Error
import Lang.PrettyPrint

data Fixity = Prefix | Closed deriving (Eq, Show)

data Precedence = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
                deriving (Eq, Enum, Ord, Show)

data Operator = Op { fixity :: Fixity
                   , precedence :: Precedence
                   , arity :: Int
                   , nameParts :: [Text]
                   } deriving (Show)

opName :: Operator -> Text
opName (Op Prefix _ _ parts) = T.concat (intersperse "_" parts ++ ["_"])
opName (Op Closed _ _ parts) = T.concat (intersperse "_" parts)

-- Operator Parsing

mkOp :: Precedence -> Text -> Maybe Operator
mkOp prec op
  | not (isValidOp op) = Nothing
  | h <- T.head op, h == '_' = Nothing
  | l <- T.last op, l == '_' = mkOp' Prefix prec op
  | otherwise = mkOp' Closed prec op
  where
    mkOp' fx prec op = Just $ uncurry (Op fx prec) (splitOp op)

splitOp :: Text -> (Int, [Text])
splitOp = (length &&& id) . filter (not . T.null) . T.split (== '_')

isValidOp :: Text -> Bool
isValidOp op
  | not ("_" `T.isInfixOf` op) = False
  | (x:xs) <- T.split (== '_') op = Maybe.isJust $ foldl isValidOp' (Just x) xs
  | otherwise = False
  where
    isValidOp' Nothing y = Nothing
    isValidOp' (Just x) y
      | not (isIdentifier x || T.null x) = Nothing
      | otherwise = Just y

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
           deriving (Show, Eq, Ord, D.Typeable, D.Data)

arrow :: Parser Token
arrow = (string "→" <|> string "->") >> return TokArrow

lambda :: Parser Token
lambda = (char 'λ' <|> char '\\') >> return TokLambda

reservedWords = [ "\\", "λ", "Box", "□", "*", "."
                , "->", "→", "?", "refl", "Id"
                , ":", "::", "elimJ" ]

illegalChars :: [Char]
illegalChars = ['(', ')', '"', '.', '{', '}']

ident :: Parser Text
ident = do
    t <- Atto.takeWhile1 (\c -> not (Char.isSpace c) && c `notElem` illegalChars)
    when (elem t reservedWords) $ fail (show t ++ " is a reserved word")
    return t

identifier :: [Operator] -> Parser Token
identifier ops = do
    i <- ident
    return $ if i `elem` (nameParts =<< ops)
             then TokNamePart i
             else TokIdentifier i

isIdentifier :: Text -> Bool
isIdentifier t = case flip feed "" $ Atto.parse ident t of
    (Done _ t') -> True
    otherwise   -> False

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

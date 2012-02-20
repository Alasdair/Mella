{-# LANGUAGE OverloadedStrings #-}

module Lang.Term.Identifier
    ( ident
    , isIdentifier
    , illegalChars
    , reservedWords
    ) where

import Control.Monad

import qualified Data.Char as Char

import Data.Attoparsec.Text as Atto
import Data.Attoparsec.Combinator as Atto
import Data.Text (Text)
import qualified Data.Text as T

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

isIdentifier :: Text -> Bool
isIdentifier t = case flip feed "" $ Atto.parse ident t of
    (Done _ t') -> True
    otherwise   -> False

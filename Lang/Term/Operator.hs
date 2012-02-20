{-# LANGUAGE OverloadedStrings #-}

module Lang.Term.Operator
    ( Fixity (..)
    , Precedence (..)
    , Operator, opName
    , fixity, precedence, arity, nameParts
    , mkOp
    , isValidOp
    ) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intersperse)
import qualified Data.Maybe as Maybe

import Lang.Term.Identifier

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

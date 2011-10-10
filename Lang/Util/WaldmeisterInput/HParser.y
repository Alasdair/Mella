{
{-# LANGUAGE OverloadedStrings #-}

module Lang.Util.WaldmeisterInput.HParser
    ( parseWMFile
    , parseFuns
    , WMFile (..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Tree

import Lang.Error
import Lang.PrettyPrint
import Lang.Util.WaldmeisterInput.Tokenizer

}

%name parseWMFile File
%name parseFuns Funs
%tokentype { Token }
%error { parseError }
%monad { Error }

%token
      name       { TokName $$ }
      mode       { TokMode $$ }
      ident      { TokIdentifier $$ }
      sorts      { TokSorts }
      ':'        { TokColon }
      '->'       { TokArrow }
      signature  { TokSignature }
      ordering   { TokOrdering }
      lpo        { TokLPO }
      kbo        { TokKBO }
      '>'        { TokGT }
      variables  { TokVariables }
      ','        { TokComma }
      equations  { TokEquations }
      '='        { TokEq }
      '('        { TokOpenBracket }
      ')'        { TokCloseBracket }
      int        { TokInt $$ }
      conclusion { TokConclusion }

%%

File :: { WMFile }
File : name
       mode
       Sorts
       signature Funs
       ordering KBO WeightsM Ordering
       variables Variables
       equations Equations
       conclusion Equations { WMFile $1 $2 $3 $5 $7 $8 $9 $11 $13 (head $15) }

Sorts :: { [Text] }
Sorts : sorts Idents { $2 }

Idents :: { [Text] }
Idents : Ident Idents { $1 : $2 }
       | Ident        { [$1] }

IdentsZ :: { [Text] }
IdentsZ : Ident IdentsZ { $1 : $2 }
        | {- empty -}  { [] }

Ident :: { Text }
Ident : ident { $1 }
      | int   { T.pack (show $1) }

Fun :: { (Text, [Text]) }
Fun : Ident ':' IdentsZ '->' Ident { ($1, $3 ++ [$5]) }

Funs :: { [(Text, [Text])] }
Funs : Fun Funs { $1 : $2 }
     | Fun      { [$1] }

KBO :: { Bool }
KBO : kbo { True }
    | lpo { False }

Ordering :: { [Text] }
Ordering : Ident '>' Ordering { $1 : $3 }
         | Ident              { [$1] }

WeightsM :: { [(Text, Int)] }
WeightsM : Weights     { $1 }
         | {- empty -} { [] }

Weights :: { [(Text, Int)] }
Weights : Weight ',' Weights { $1 : $3 }
        | Weight             { [$1] }

Weight :: { (Text, Int) }
Weight : Ident '=' int { ($1, $3) }

VList :: { [Text] }
VList : Ident ',' VList { $1 : $3 }
      | Ident           { [$1] }

VSet :: { (Text, [Text]) }
VSet : VList ':' Ident { ($3, $1) }

Variables :: { [(Text, [Text])] }
Variables : VSet Variables { $1 : $2 }
          | VSet           { [$1] }

ITree :: { Tree Text }
ITree : Ident '(' IForest ')' { Node $1 $3 }
      | Ident                 { Node $1 [] }

IForest :: { Forest Text }
IForest : ITree ',' IForest { $1 : $3 }
        | ITree             { [$1] }

Equation :: { (Tree Text, Tree Text) }
Equation : ITree '=' ITree { ($1, $3) }

Equations :: { [(Tree Text, Tree Text)] }
Equations : Equation Equations { $1 : $2 }
          | Equation           { [$1] }
{

data WMFile = WMFile
    { wmName :: Text
    , wmMode :: Text
    , wmSorts :: [Text]
    , wmSig :: [(Text, [Text])]
    , wmKBO :: Bool
    , wmWeights :: [(Text, Int)]
    , wmOrdering :: [Text]
    , wmVariables :: [(Text, [Text])]
    , wmEquations :: [(Tree Text, Tree Text)]
    , wmConclusion :: (Tree Text, Tree Text)
    } deriving (Show)

data ParseError = PErr [Token]

instance ErrorMsg ParseError where
    toPrettyText (PErr tokens) sch =
        T.concat [ colorBanner sch errorColor "PARSE ERROR"
                 , "\nParser failed when parsing tokens:\n"
                 , T.pack (show (take 10 tokens))
                 ]

parseError :: [Token] -> Error a
parseError = throwError . PErr

}

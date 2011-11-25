{
{-# LANGUAGE OverloadedStrings #-}

module Lang.Term.HParser 
    ( sugarParse
    , sugarParseVarList
    , SugarTerm (..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Lang.Error
import Lang.PrettyPrint
import Lang.Term
import Lang.Term.Tokenizer

}

%name sugarParse TermB
%name sugarParseVarList VarList
%tokentype { Token }
%error { parseError }
%monad { Error }

%token
      '->'           { TokArrow }
      lam            { TokLambda }
      '('            { TokOpenBracket }
      ')'            { TokCloseBracket }
      ident          { TokIdentifier $$ }
      ':'            { TokHasType }
      id             { TokId }
      axiomJ         { TokJ }
      refl           { TokRefl }
      natType        { TokNat }
      nat            { TokNumeric $$ }
      sort           { TokSort $$ }
      '::'           { TokAnnotate }
      '?'            { TokMeta }

%%

TermB :: { SugarTerm }
TermB : TySig                 { $1 }
      | TermBNT '->' TermB    { STySig ["_"] $1 $3 }
      | lam Idents '->' TermB { SLam $2 $4 }
      | TermBNT               { $1 }

TermBNT :: { SugarTerm }
TermBNT : App               { $1 }
        | id Term Term Term { SId $2 $3 $4 }
        | TermNB            { $1 }
        | Term '::' Term    { SAnnotate $1 $3 }
        | '(' TermB ')'     { $2 }

TermNB :: { SugarTerm }
TermNB : ident   { SVar $1 }
       | nat     { SNatLiteral $1 }
       | natType { SNat }
       | sort    { SSort $1 }
       | '?'     { SMeta }
       | AxiomJ  { $1 }
       | refl    { SRefl }

AxiomJ :: { SugarTerm }
AxiomJ : axiomJ Term Term Term Term Term Term { SJ $2 $3 $4 $5 $6 $7 }

Term :: { SugarTerm }
Term : '(' TermB ')' { $2 }
     | TermNB        { $1 }

App :: { SugarTerm }
App : App Term  { SApp $1 $2 }
    | Term Term { SApp $1 $2 }

TySig :: { SugarTerm }
TySig : '(' Idents ':' TermB ')' TySig      { STySig $2 $4 $6 }
      | '(' Idents ':' TermB ')' '->' TermB { STySig $2 $4 $7 }

Idents :: { [Text] }
Idents : ident Idents { $1 : $2 }
       | ident        { [$1] }

VarList :: { [SugarTerm] }
VarList : Term VarList { $1 : $2 }
        | Term         { [$1] }

{

data SugarTerm = SLam [Text] SugarTerm
               | SApp SugarTerm SugarTerm
               | SId SugarTerm SugarTerm SugarTerm
               | SJ SugarTerm SugarTerm SugarTerm SugarTerm SugarTerm SugarTerm
               | SRefl
               | STySig [Text] SugarTerm SugarTerm
               | SVar Text
               | SNatLiteral Int
               | SNat
               | SAnnotate SugarTerm SugarTerm
               | SSort Sort
               | SMeta
               deriving (Show)

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

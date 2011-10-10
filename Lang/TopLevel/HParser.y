{
{-# LANGUAGE OverloadedStrings #-}

module Lang.TopLevel.HParser where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as T

import Lang.TopLevel.Tokenizer
}

%name parseCommand Cmd
%name parseCommands TCmds
%tokentype { Token }
%error { parseError }
%monad { Maybe }

%token
      fun     { TokFun }
      theorem { TokTheorem }
      refl    { TokRefl }
      ':'     { TokColon }
      '.'     { TokPeriod }
      quote   { TokQuote $$ }
      ident   { TokIdent $$ }
      term    { TokTerm $$ }
      '('     { TokOpen }
      ')'     { TokClose }
      keyword { TokKeyword $$ }

%%

Cmds :: { [Command] }
Cmds : Cmd Cmds { $1 : $2 }
     | Cmd      { [$1] }

TCmds :: { [Command] }
TCmds : Cmds        { $1 }
      | {- empty -} { [] }

Cmd :: { Command }
Cmd : fun ident ':' term term '.' { CmdFun $2 $4 $5 }
    | theorem ident ':' term '.'  { CmdTheorem $2 $4 }
    | refl '.'                    { CmdRefl }
    | term '.'                    { CmdTerm $1 }
    | ident Exprs Keywords '.'    { CmdExpr $1 $2 (Map.fromList $3) }

Exprs :: { [Expr] }
Exprs : Expression Exprs  { $1 : $2 }
      | {- empty -}       { [] }

Expression :: { Expr }
Expression : term          { ExprTerm $1 }
           | ident         { ExprIdent $1 }
           | quote         { ExprString $1 }
           | '(' Exprs ')' { ExprList $2 }

Keywords :: { [(Text, [Expr])] }
Keywords : Keyword Keywords { $1 : $2 }
         | {- empty -}      { [] }

Keyword :: { (Text, [Expr]) }
Keyword : keyword Exprs { ($1, $2) }

{

data Command = CmdTerm Text
             | CmdTheorem Text Text
             | CmdFun Text Text Text
             | CmdRefl
             | CmdExpr Text [Expr] (Map Text [Expr])
             deriving (Show)

data Expr = ExprIdent Text
          | ExprTerm Text
          | ExprString Text
          | ExprList [Expr]
          deriving (Show)

parseError :: [Token] -> Maybe a
parseError _ = Nothing

}

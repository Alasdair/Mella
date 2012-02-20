{-# LANGUAGE OverloadedStrings #-}

module Lang.TopLevel.Command.EqReasoning (eqCommands) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Attoparsec.Combinator as Atto

import qualified Lang.Term.Identifier as Identifier
import Lang.PrettyPrint
import Lang.TopLevel.Parser
import Lang.TopLevel.Monad

data EqExpr = EqExpr Text [EqExpr] deriving (Show)

eqCommands :: Map Text CommandImpl
eqCommands = commandsFromList [ implEq ]

parseEqExpr'' :: Atto.Parser EqExpr
parseEqExpr'' = do { v <- Identifier.ident
                   ; Atto.skipSpace
                   ; return (EqExpr v [])
                   }
                <|>
                do { Atto.char '('
                   ; f <- Identifier.ident
                   ; Atto.skipSpace
                   ; args <- Atto.many1 parseEqExpr''
                   ; Atto.char ')'
                   ; return (EqExpr f args)
                   }

parseEqExpr' = do { f <- Identifier.ident
                  ; Atto.skipSpace
                  ; args <- Atto.many1 parseEqExpr''
                  ; return (EqExpr f args)
                  }
               <|> parseEqExpr''

parseLoc :: Atto.Parser [Int]
parseLoc = do { n <- read <$> Atto.many1 Atto.digit
               ; Atto.char ','
               ; ns <- parseLoc
               ; return (n : ns)
               }
           <|>
           do { n <- read <$> Atto.many1 Atto.digit
              ; return [n]
              }
           <|> return []

parseDir :: Atto.Parser Bool
parseDir = (Atto.string "RL" >> return True) <|> (Atto.string "LR" >> return False) <|> return False

parseLocDir' :: Atto.Parser ([Int], Bool)
parseLocDir' = do { loc <- parseLoc; dir <- parseDir; Atto.endOfInput; return (loc, dir) }

parseLocDir text = case flip Atto.feed "" (Atto.parse parseLocDir' text) of
                     Atto.Done _ ld -> return ld
                     otherwise -> errorMsg (T.concat ["could not parse ", text])

parseEqExpr :: Text -> TopLevel EqExpr
parseEqExpr text = case flip Atto.feed "" (Atto.parse parseEqExpr' text) of
                     Atto.Done _ expr -> return expr
                     otherwise -> errorMsg (T.concat ["could not parse expression: ", text])

eqExprToText :: EqExpr -> Text
eqExprToText (EqExpr v []) = v
eqExprToText (EqExpr f args) = T.concat ["(", f, " ", T.concat (intersperse " " (map eqExprToText args)), ")"]

eqExprToTextNB :: EqExpr -> Text
eqExprToTextNB (EqExpr v []) = v
eqExprToTextNB (EqExpr f args) = T.concat [f, " ", T.concat (intersperse " " (map eqExprToText args))]

tlNth :: Int -> [a] -> TopLevel a
tlNth 0 (x:xs) = return x
tlNth n (x:xs) = tlNth (n - 1) xs
tlNth _ []     = errorMsg "list index out of range!"

subExpr :: [Int] -> EqExpr -> TopLevel EqExpr
subExpr [] expr = return expr
subExpr (0:ns) expr = subExpr ns expr
subExpr (n:ns) (EqExpr f args) = subExpr ns =<< tlNth (n - 1) args

congExpr :: [Int] -> EqExpr -> TopLevel EqExpr
congExpr [] expr = return $ EqExpr "eq-cong-var" []
congExpr (0:ns) expr = congExpr ns expr
congExpr (n:ns) (EqExpr f args) = do
    expr <- congExpr ns =<< tlNth (n - 1) args
    return $ EqExpr f (take (n -1) args ++ expr : drop n args)


implEq :: CommandImpl
implEq = Impl { implName = "="
              , implFun  = eq
              , implDoc  = "Equational Reasoning"
              }

eq :: [Expr] -> Opts -> TopLevel ()
eq [ExprTerm to, ExprIdent "by", ExprTerm by] opts =
    runCommand (CmdExpr "=" [ExprTerm to, ExprIdent "by", ExprTerm by, ExprIdent "at", ExprString ""] opts)

eq [ExprTerm to, ExprIdent "by", ExprTerm by, ExprIdent "at", ExprString locText] _ = do
    (tyTerm, fromTerm, finalToTerm) <- getCurrentEquation

    let ty      = pretty noScheme tyTerm
        from    = pretty noScheme fromTerm
        finalTo = pretty noScheme finalToTerm

    fromEq <- parseEqExpr from
    toEq <- parseEqExpr to

    (loc, rightToLeft) <- parseLocDir locText

    fromEqCong <- subExpr loc fromEq
    toEqCong <- subExpr loc toEq
    congF <- congExpr loc fromEq

    let termSym   = if not rightToLeft then by
                    else T.concat ["sym (", ty, ") "
                                  , eqExprToText toEqCong, " ", eqExprToText fromEqCong
                                  , " (", by, ")"]
        termCong  = T.concat ["cong (", ty, ") (", ty, ") "
                             , eqExprToText fromEqCong, " ", eqExprToText toEqCong, " ("
                             , "\\eq-cong-var -> ", eqExprToTextNB congF, ") (", termSym, ")"]
        termTrans = T.concat ["trans (", ty, ") (", from, ") (", to, ") (", finalTo, ") (", termCong, ") ?"]

    runCommand (CmdTerm termTrans)

{-# LANGUAGE OverloadedStrings #-}

module Lang.Term.Parser (parseTerm) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad

import qualified Data.Char as Char
import Data.Ord (comparing)
import Data.List (foldl', intersperse)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Word

import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Identity
import qualified Text.Parsec as Psc
import qualified Text.Parsec.Pos as Psc
import qualified Text.Parsec.Error as Psc

import Lang.Term
import Lang.Error
import Lang.PrettyPrint
import Lang.Term.Tokenizer
import Lang.Term.Operator

type Parser a = Psc.Parsec [Token] [Operator] a

-- +----------------------------------------------------------------+
-- + 1. Recognizing Tokens.                                         +
-- +----------------------------------------------------------------+

-- Short aliases for simple Token parsers.
tLAM, tARR, tOB, tCB, tOT, tCT, tCOLON, tMETA, tREFL, tJ, tID, tANN :: Parser ()
tLAM = isToken TokLambda
tARR = isToken TokArrow
tOB = isToken TokOpenBracket
tCB = isToken TokCloseBracket
tOT = isToken TokOpenType
tCT = isToken TokCloseType
tCOLON = isToken TokHasType
tMETA = isToken TokMeta
tREFL = isToken TokRefl
tJ = isToken TokJ
tID = isToken TokId
tANN = isToken TokAnnotate

-- Match a token of the same type as the provided token. Returns the
-- matched token.
matchToken :: Token -> Parser Token
matchToken x = Psc.token show (const $ Psc.newPos "N/A" 0 0) (sameType x)
  where
    sameType (TokIdentifier _) y@(TokIdentifier _) = Just y
    sameType (TokSortBox _) y@(TokSortBox _) = Just y
    sameType x y = if x == y then Just y else Nothing

-- Match a token which is exactly the same as the provided token.
isToken :: Token -> Parser ()
isToken x = Psc.token show (const $ Psc.newPos "N/A" 0 0) (\y -> if x == y then Just () else Nothing)

-- Parse an identifier.
ident :: Parser Text
ident = unIdent <$> matchToken (TokIdentifier "")

idents :: Parser [Text]
idents = map unIdent <$> (Psc.many1 . matchToken $ TokIdentifier "")

sort :: Parser Sort
sort = (matchToken TokSortStar >> return Star) <|> (Box . unBox <$> matchToken (TokSortBox 0))

namePart :: Text -> Parser ()
namePart n = isToken (TokNamePart n)

-- +----------------------------------------------------------------+
-- + 2. Desugaring.                                                 +
-- +----------------------------------------------------------------+

-- Terms are parsed into a form which closely resembles the syntax of
-- the language. For example, lambdas can have multiple arguments in
-- Mella, but this is syntactic sugar for nested lambdas. The sugared
-- term, or STerm datatype represents terms with syntactic sugar
-- included, they can be desugared with the desugar function.
data STerm = SLam [Text] STerm
               | SApp STerm STerm
               | SId STerm STerm STerm
               | SJ STerm STerm STerm STerm STerm STerm
               | SRefl
               | STySig [Text] STerm STerm
               | SVar Text
               | SAnnotate STerm STerm
               | SSort Sort
               | SMeta
               | SIfThenElse STerm STerm STerm
               | SMult STerm STerm
               deriving (Show)

desugar :: STerm -> Term
desugar (SLam args expr) = foldr Lam (desugar expr) (map Tag args)
desugar (SApp f x) = App (desugar f) (desugar x)
desugar (SId ty t1 t2) = Id (desugar ty) (desugar t1) (desugar t2)
desugar SRefl = Refl
desugar (SJ a b c d e f) = J (desugar a) (desugar b) (desugar c) (desugar d) (desugar e) (desugar f)
desugar (SVar v) = Named v
desugar (SAnnotate t ty) = Ann (desugar t) (desugar ty)
desugar (SSort s) = Sort s
desugar SMeta = Meta 0
desugar (STySig args s t) = foldr (\tag t -> (Pi tag (desugar s) t)) (desugar t) (map Tag args)

-- +----------------------------------------------------------------+
-- + 3. Term Parsing.                                               +
-- +----------------------------------------------------------------+

term :: Parser STerm
term = do {tOB; t <- termB; tCB; return t} <|> atom

-- termB parses any term which is required to be contained within
-- brackets when within another term.
termB :: Parser STerm
termB = Psc.choice
  [ do {tLAM; vars <- idents; tARR; t <- termB; return (SLam vars t)}
  , tySig
  , do { t <- term11
       ; op <- op11
       ; return $ case op of
           Op11Arr t' -> STySig ["_"] t t'
           Op11Ann t' -> SAnnotate t t'
           Op11Ep -> t
       }
  ]

data Op11 = Op11Arr STerm | Op11Ann STerm | Op11Ep

op11 = Psc.choice
  [ do {tARR; t <- termB; return (Op11Arr t)}
  , do {tANN; t <- termB; return (Op11Ann t)}
  , return Op11Ep
  ]

term0 :: Parser STerm
term0 = Psc.choice
  [ do {tID; ty <- term; t1 <- term; t2 <- term; return (SId ty t1 t2)}
  , axiomJ
  , Psc.chainl1 term (return SApp)
  ]

selectOps :: Precedence -> Parser [Operator]
selectOps p = filter (\x -> precedence x == p) <$> Psc.getState

data NonEmpty a = NE {nhead :: a, ntail :: [a]}

{- Experiments with operator parsing...

termP :: Precedence -> Parser STerm
termP prec = do
    ops <- selectOps prec
    Psc.choice (map (operator next) ops) <|> next
  where
    next | prec == One = term0
         | otherwise   = termP (pred prec)

operator :: Parser STerm -> Operator -> Parser STerm
operator next op = operatorArgs next op >>= return . foldl SApp (SVar (opName op))

operatorArgs :: Parser STerm -> Operator -> Parser [STerm]
operatorArgs next op
  | fixity op == Closed = parseParts
  | fixity op == Prefix = do {ts <- parseParts; t <- next; return (ts ++ [t])}
  where
    parseParts = Maybe.catMaybes <$> sequence (intersperse (Just <$> next) (map np (nameParts op)))

    np x = Nothing <$ namePart x
-}

term11 :: Parser STerm
term11 = term0

-- In our parse trees atoms are those terms that do not have any
-- subterms. They include variable identifiers, sorts, metavariables and
-- refl.
atom :: Parser STerm
atom = Psc.choice
  [ SVar <$> ident
  , SSort <$> sort
  , tMETA >> return SMeta
  , tREFL >> return SRefl
  ]

axiomJ :: Parser STerm
axiomJ = do {tJ; a <- term; b <- term; c <- term; d <- term; e <- term; f <- term; return (SJ a b c d e f)}

tySig :: Parser STerm
tySig = do
  tOT
  i <- idents
  tCOLON
  ty <- termB
  tCT
  t <- (tARR >> termB) <|> tySig
  return (STySig i ty t)

testOps = [ite, brk]
  where
    Just ite = mkOp Ten "if_then_else_"
    Just brk = mkOp Nine "[_=[_]=_]"

parseSTerm :: Parser STerm -> [Operator] -> [Token] -> Error STerm
parseSTerm parser ops toks =
    case Psc.runParser (parser >>= \x -> Psc.eof >> return x) ops "N/A" toks of
      Right x -> return x
      Left err -> throwError (head (Psc.errorMessages err))

--TODO: Improve error messages.

instance ErrorMsg Psc.Message where
    toPrettyText msg sch = T.pack $ Psc.messageString msg

-- | parse a Term within the provided Ctx.
parseTerm :: Ctx -> [Operator] -> Text -> Error Term
parseTerm ctx ops t =
    tokenize ops t >>= parseSTerm termB ops >>= return . assignVars ctx . desugar

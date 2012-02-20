{-# LANGUAGE CPP, OverloadedStrings #-}

module Lang.Term.MParser where

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
import Lang.Term.Mixfix

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

type TParser a = Psc.Parsec [Token] [Operator] a

matchToken :: Token -> TParser Token
matchToken x = Psc.token show (const $ Psc.newPos "N/A" 0 0) (sameType x)
  where
    sameType (TokIdentifier _) y@(TokIdentifier _) = Just y
    sameType (TokSortBox _) y@(TokSortBox _) = Just y
    sameType x y = if x == y then Just y else Nothing

isToken :: Token -> TParser ()
isToken x = Psc.token show (const $ Psc.newPos "N/A" 0 0) (\y -> if x == y then Just () else Nothing)

ident :: TParser Text
ident = unIdent <$> matchToken (TokIdentifier "")

idents :: TParser [Text]
idents = map unIdent <$> (Psc.many1 . matchToken $ TokIdentifier "")

sort :: TParser Sort
sort = (matchToken TokSortStar >> return Star) <|> (Box . unBox <$> matchToken (TokSortBox 0))

namePart :: Text -> TParser ()
namePart n = isToken (TokNamePart n)

data SugarTerm = SLam [Text] SugarTerm
               | SApp SugarTerm SugarTerm
               | SId SugarTerm SugarTerm SugarTerm
               | SJ SugarTerm SugarTerm SugarTerm SugarTerm SugarTerm SugarTerm
               | SRefl
               | STySig [Text] SugarTerm SugarTerm
               | SVar Text
               | SAnnotate SugarTerm SugarTerm
               | SSort Sort
               | SMeta
               | SIfThenElse SugarTerm SugarTerm SugarTerm
               | SMult SugarTerm SugarTerm
               deriving (Show)

term :: TParser SugarTerm
term = do {tOB; t <- termB; tCB; return t} <|> termLeaf

-- termB parses any term which is required to be contained within
-- brackets when within another subterm.
termB :: TParser SugarTerm
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

data Op11 = Op11Arr SugarTerm | Op11Ann SugarTerm | Op11Ep

op11 = Psc.choice
  [ do {tARR; t <- termB; return (Op11Arr t)}
  , do {tANN; t <- termB; return (Op11Ann t)}
  , return Op11Ep
  ]

term0 :: TParser SugarTerm
term0 = Psc.choice
  [ do {tID; ty <- term; t1 <- term; t2 <- term; return (SId ty t1 t2)}
  , axiomJ
  , Psc.chainl1 term (return SApp)
  ]

selectOps :: Precedence -> TParser [Operator]
selectOps p = filter (\x -> precedence x == p) <$> Psc.getState

data NonEmpty a = NE {nhead :: a, ntail :: [a]}

termP :: Precedence -> TParser SugarTerm
termP prec = do
    ops <- selectOps prec
    Psc.choice (map (operator next) ops) <|> next
  where
    next | prec == One = term0
         | otherwise   = termP (pred prec)

operator :: TParser SugarTerm -> Operator -> TParser SugarTerm
operator next op = operatorArgs next op >>= return . foldl SApp (SVar (opName op))

operatorArgs :: TParser SugarTerm -> Operator -> TParser [SugarTerm]
operatorArgs next op
  | fixity op == Closed = parseParts
  | fixity op == Prefix = do {ts <- parseParts; t <- next; return (ts ++ [t])}
  where
    parseParts = Maybe.catMaybes <$> sequence (intersperse (Just <$> next) (map np (nameParts op)))

    np x = Nothing <$ namePart x

term11 :: TParser SugarTerm
term11 = term0

-- In our parse trees atoms are those terms that do not have any
-- subterms. They include variable identifiers, sorts, metavariables and
-- refl.
termLeaf :: TParser SugarTerm
termLeaf = Psc.choice
  [ SVar <$> ident
  , SSort <$> sort
  , tMETA >> return SMeta
  , tREFL >> return SRefl
  ]

axiomJ :: TParser SugarTerm
axiomJ = do {tJ; a <- term; b <- term; c <- term; d <- term; e <- term; f <- term; return (SJ a b c d e f)}

tySig :: TParser SugarTerm
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

sugarParse :: TParser SugarTerm -> [Operator] -> [Token] -> Error SugarTerm
sugarParse parser ops toks =
    case Psc.runParser (parser >>= \x -> Psc.eof >> return x) ops "N/A" toks of
      Right x -> return x
      Left err -> throwError (head (Psc.errorMessages err))

instance ErrorMsg Psc.Message where
    toPrettyText msg sch = T.pack $ Psc.messageString msg

desugar :: SugarTerm -> Term
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

-- | parse a Term within the provided Ctx.
parseTerm :: TParser SugarTerm -> Ctx -> [Operator] -> Text -> Error Term
parseTerm parser ctx ops t = tokenize ops t >>= sugarParse parser ops >>= return . assignVars ctx . desugar

test p t = let x = runError noScheme $ parseTerm p emptyCtx testOps t
           in do
               print x
               let (Right t) = x
               prettyPrint noScheme t

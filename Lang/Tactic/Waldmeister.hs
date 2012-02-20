{-# LANGUAGE OverloadedStrings, CPP #-}

module Lang.Tactic.Waldmeister (WaldmeisterResult (..), waldmeister) where

import Control.Arrow (first, second, (&&&), (|||))
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import qualified Control.Exception as C

import Data.Char (isSpace, isDigit, isAlphaNum, chr)
import Data.List (nub, intersperse, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)

import Data.Attoparsec.Text as Atto

import System.Process
import System.Timeout
import System.Random
import System.IO
import System.Exit

import Lang.Util.OMap (OMap)
import qualified Lang.Util.OMap as OMap

import Lang.Term
import Lang.PrettyPrint
import qualified Lang.Term.Parser as Parser
import Lang.TypeChecker.Monad
--import Lang.Tactic.Tactic

#define __ERROR__ typeError "Lang.Tactic.Waldmeister" __LINE__

data WaldmeisterResult = WMTimeout
                       | WMRefuted
                       | WMTerm Term UTCTime NominalDiffTime
                       | WMError Int

waldmeister :: [Term] -> [Term] -> Int -> Bool -> Term -> TCMT IO WaldmeisterResult
waldmeister axioms' signature time kbo conclusion@(Id ty t1 t2) = do
    validType ty
    t1 `hasType` ty
    t2 `hasType` ty

    axiomTypes <- mapM infer axioms'
    axiomExprs <- mapM axiomize axiomTypes
    signatureNames <- mapM sigName signature
    signatureTypes <- mapM infer signature

    signatureText <- zip signatureNames <$> validSigs (map (sig 0) signatureTypes)
    let kinds = nub . concat $ map snd signatureText

    conclusionExpr <- axiomize conclusion

    -- liftIO $ mapM print axiomExprs

    let wmFile = T.concat [ header
                          , "SORTS\n", T.concat (intersperse " " kinds), "\n"
                          , outputSignatures kbo signatureText
                          , outputWAxioms axiomExprs
                          , outputConclusion conclusionExpr
                          ]

    liftIO $ do
        T.writeFile "/tmp/proof.pr" wmFile
        -- T.putStrLn wmFile
        putStrLn "Calling waldmeister..."

    let cmd = "/usr/local/bin/waldmeister"
        args = ["--details", "/tmp/proof.pr"]

    -- The code to run the waldmeister process is adapted from the base GHC libraries.
    (Just inh, Just outh, Just errh, pid) <- liftIO $ createProcess (proc cmd args) { std_in  = CreatePipe
                                                                                    , std_out = CreatePipe
                                                                                    , std_err = CreatePipe
                                                                                    }

    startTime <- liftIO getCurrentTime
    (out, err, exitCode, timedOut) <- liftIO $ do
        timeoutMVar <- newEmptyMVar


        threadid <- forkIO $ do
          threadDelay (time * 1000000)
          putMVar timeoutMVar ()
          terminateProcess pid

        outMVar <- newEmptyMVar

        -- fork off a thread to start consuming stdout
        out  <- hGetContents outh
        forkIO $ C.evaluate (length out) >> putMVar outMVar ()

        -- fork off a thread to start consuming stderr
        err  <- hGetContents errh
        forkIO $ C.evaluate (length err) >> putMVar outMVar ()

        takeMVar outMVar >> takeMVar outMVar
        hClose inh >> hClose outh >> hClose errh

        exitCode <- waitForProcess pid

        timedOut <- not <$> isEmptyMVar timeoutMVar

        return (out, err, exitCode, timedOut)

    endTime <- liftIO getCurrentTime
    let waldmeisterTime = diffUTCTime endTime startTime

    case (exitCode, timedOut) of
      (_, True) -> return WMTimeout
      (ExitFailure n, _) -> return $ WMError n
      otherwise -> do
          liftIO $ putStrLn out
          if "refuted" `T.isInfixOf` (T.pack out) then return WMRefuted
          else do
              let proofOutput = T.unlines . dropWhile (not . T.isPrefixOf "Consider the following") $ T.lines (T.pack out)
                  presult = flip feed "" $ parse parseWMProof proofOutput
                  (Done _ proof) = presult

              proofTerm <- rcWMProof proof
              liftIO $  putStr err

              return (WMTerm proofTerm startTime waldmeisterTime)
  where
    validSigs :: (Functor m, Monad m) => [Maybe a] -> TCMT m [a]
    validSigs [] = return []
    validSigs (Just x : xs) = (x:) <$> validSigs xs
    validSigs (Nothing : _) = __ERROR__ "validSigs" [] "Not a valid signature"

    sigName :: (Functor m, Monad m) => Term -> TCMT m Text
    sigName (Unnamed (DB n _)) = return $ T.append "s" (T.pack (show n))
    sigName s = __ERROR__ "sigName" [("s", s)] "not a valid signature name\n{s}"

waldmeister _ _ _ _ k = do
    liftIO $ prettyPrint debugScheme k
    __ERROR__ "waldmeister" [] "invalid args passed to waldmeister"

header = "NAME testproof\nMODE PROOF\n"

data WExpr = WFun Int [WExpr]
           | WVar Int
           deriving (Show)

outputConclusion :: WAxiom -> Text
outputConclusion (WAxiom _ expr1 expr2) =
    T.concat ["CONCLUSION\n", outputWExpr [] expr1, " = ", outputWExpr [] expr2]

decAllUnnameds :: Int -> Term -> Term
decAllUnnameds n (Unnamed (DB m name)) = Unnamed $ DB (m - 1) name
decAllUnnameds n x = x

data WAxiom = WAxiom [(Tag, Term)] WExpr WExpr deriving (Show)

outputWAxioms :: [WAxiom] -> Text
outputWAxioms axioms =
    let axioms' = map outputWAxiom axioms
        vars = Map.unionsWith (++) $ map fst axioms'
        axioms'' = map snd axioms'
    in T.concat [ "VARIABLES\n"
                , T.concat (intersperse "\n" (outputVars vars))
                , "\nEQUATIONS\n"
                , T.concat (intersperse "\n" axioms'')
                , "\n"
                ]
  where
    outputVars :: Map Text [Text] -> [Text]
    outputVars vars =
        map (\(k, vs) -> T.concat [T.concat (intersperse "," (nub vs)), " : ", k]) $ Map.toList vars

outputWAxiom :: WAxiom -> (Map Text [Text], Text)
outputWAxiom (WAxiom args expr1 expr2) =
    let args' = processArgs args
        text1 = outputWExpr (map snd args') expr1
        text2 = outputWExpr (map snd args') expr2
        args'' = Map.fromListWith (++) $ map (second pure) args'
    in (args'', T.concat [text1, " = ", text2])

outputWExpr :: [Text] -> WExpr -> Text
outputWExpr args (WVar n) = args !! n
outputWExpr args (WFun n []) = T.append "s" (T.pack (show n))
outputWExpr args (WFun n exprs) =
    T.concat ["s", T.pack (show n),  "(", T.concat (intersperse "," (map (outputWExpr args) exprs)), ")"]

varKindName (Unnamed (DB n _)) = T.append "K" (T.pack (show n))

processArgs :: [(Tag, Term)] -> [(Text, Text)]
processArgs args = processArgs' Map.empty (map snd args)
  where
    processArgs' :: Map Text Int -> [Term] -> [(Text, Text)]
    processArgs' kinds [] = []
    processArgs' kinds (t:ts) =
        let k = varKindName t
            n = Map.findWithDefault 0 k kinds
        in (k, T.concat ["v", T.pack (show n), k]) : processArgs' (Map.insertWith (+) k 1 kinds) ts

axiomize :: Term -> TCMT IO WAxiom
axiomize t = do
    WAxiom args expr1 expr2 <- axiomize' =<< tnf t
    return $ WAxiom args (mapWVars expr1) (mapWVars expr2)
  where
    mapWVars (WFun n exprs) = WFun n (map mapWVars exprs)
    mapWVars (WVar n) = WVar (abs n - 1)

decAllUnnamed :: Int -> Term -> Term
decAllUnnamed n (Unnamed (DB m name)) = Unnamed $ DB (m - 1) name
decAllUnnamed n x = x

axiomize' :: Term -> TCMT IO WAxiom
axiomize' (Pi tag s t) = do
    WAxiom args expr1 expr2 <- axiomize' (cata 0 decAllUnnamed t)
    return $ WAxiom ((tag, s) : args) expr1 expr2
axiomize' (Id _ t1 t2) = do
    w1 <- toWExpr t1
    w2 <- toWExpr t2
    return $ WAxiom [] w1 w2

toWExpr :: (Functor m, Monad m) => Term -> TCMT m WExpr
toWExpr (Unnamed (DB n _)) | n < 0 = return $ WVar n
toWExpr (Unnamed (DB n _)) = return $ WFun n []
toWExpr (App t1 t2) = do
    r <- toWExpr t1
    case r of
      WVar _ -> __ERROR__ "toWExpr" [("t1", t1)] "must be a function\n{t1}"
      WFun n exprs -> toWExpr t2 >>= (\e -> return $ WFun n (exprs ++ [e]))
toWExpr t = __ERROR__ "toWExpr" [("t", t)] "not valid in a waldmeister expression\n{t}"

outputSignatures :: Bool -> [(Text, [Text])] -> Text
outputSignatures kbo sigs =
    let sigs' = map (\(name, args) -> T.concat [name, ": ", outputSig args]) sigs
    in T.concat [ "SIGNATURE\n"
                , T.concat (intersperse "\n" sigs')
                , if kbo -- bit of a hack
                     then T.concat [ "\nORDERING\nKBO\n"
                                   , T.concat (intersperse "," (map (\t -> T.append t "=1") (map fst sigs)))
                                   , "\n"
                                   ]
                     else "\nORDERING\nLPO\n"
                , T.concat (intersperse " > " (map fst sigs))
                , "\n"
                ]
  where
    outputSig :: [Text] -> Text
    outputSig t = T.concat . intersperse " " $ init t ++ ["->"] ++ [last t]


sig :: Int -> Term -> Maybe [Text]
sig m (Unnamed (DB n _)) | n < m = Nothing
sig m (Unnamed (DB n _)) = Just [T.append "K" (T.pack (show (n - m)))]
sig m (Pi _ s t) = do
    sw <- sig m s
    tw <- sig (m + 1) t
    return $ sw ++ tw
sig _ _ = Nothing

-- +----------------------------------------------------------------+
-- + 1. Parsing Waldmeister Output                                  +
-- +----------------------------------------------------------------+
-- Code here orginally from Simon Foster's Agda integration.

optionMaybe :: (Monad f, Alternative f) => f a -> f (Maybe a)
optionMaybe p = option Nothing (liftM Just p)

spaces :: Parser ()
spaces = Atto.takeWhile isSpace >> return ()

data Expr = Con Int [Expr]
          | Var Text
          | Cnst Int deriving (Show, Eq, Ord)

type WMEquation = (Expr, Expr)

data Rule     = Ax Int | Lem Int deriving (Eq, Ord, Show)
-- data RuleDir  = LR | RL deriving (Eq, Ord, Show)
type RuleDir = Bool

-- An application includes the rule being applied and the direction
data Appli    = Appli { appliRule  :: Rule
                      , appliLR    :: RuleDir
                      , appliWhere :: [Int]
                      , appliSubst :: [(Int, Expr)]
                      } deriving Show

-- A rewrite is an application together with the resultant expression
type Rewrite  = (Appli, Expr)

data WMLemma = Lemma { lemProves :: WMEquation
                     , lemSteps  :: [Rewrite]
                     } deriving Show

type WMTheorem = WMLemma

data WMProof = WMProof { axioms  :: [WMEquation]
                       , lemmas  :: [WMLemma]
                       , theorem :: WMTheorem
                       } deriving Show

parseWMEquation :: Parser WMEquation
parseWMEquation = do{ x <- parseWMExpr
                    ; spaces
                    ; char '='
                    ; spaces
                    ; y <- parseWMExpr
                    ; return (x,y)
                    }

parseWMExpr :: Parser Expr
parseWMExpr = do{ char 's'
                ; v <- many1 digit
                ; es <- optionMaybe $
                  do{ char '('
                    ; es <- sepBy1 parseWMExpr (char ',')
                    ; char ')'
                    ; return es
                    }
                ; return $ Con (read v) (fromMaybe [] es)
                }

             <|>

             do { string "const"
                ; n <- many1 digit
                ; return $ Cnst (read n)
                }

             <|>

             do { v <- takeWhile1 isAlphaNum
                ; return (Var v)
                }


parseTheoremHead :: Parser WMEquation
parseTheoremHead = do{ string "Theorem "
                     ; many1 digit
                     ; string ":"
                     ; spaces
                     ; e <- parseWMEquation
                     ; spaces
                     ; return e
                     } <?> "parseTheoremHead"

parseTheorem :: Parser WMTheorem
parseTheorem = do{ h <- parseTheoremHead
                 ; spaces
                 ; parseWMExpr
                 ; spaces
                 ; rs <- many $ do{ a <- parseAppli
                                  ; spaces
                                  ; e <- parseWMExpr
                                  ; spaces
                                  ; return (a,e)
                                  }
                 ; return $ Lemma h rs
                 } <?> "parseTheorem"

parseLemma :: Parser WMLemma
parseLemma = do{ h <- parseLemmaHead
               ; spaces
               ; parseWMExpr
               ; spaces
               ; rs <- many $ do { a <- parseAppli
                                 ; spaces
                                 ; e <- parseWMExpr
                                 ; spaces
                                 ; return (a,e)
                                 }
               ; return $ Lemma h rs
               } <?> "parseLemma"

parseLemmaHead :: Parser WMEquation
parseLemmaHead = do{ string "Lemma "
                   ; many1 digit
                   ; string ":"
                   ; spaces
                   ; e <- parseWMEquation
                   ; spaces
                   ; return e
                   } <?> "parseLemmaHead"

parseAppli :: Parser Appli
parseAppli = do{ char '='
               ; spaces
               ; string "by"
               ; spaces
               ; r <- do{ string "Axiom"
                        ; spaces
                        ; n <- many1 digit
                        ; return $ Ax (read n)
                        }
                      <|>
                      do{ string "Lemma"
                        ; spaces
                        ; n <- many1 digit
                        ; return $ Lem (read n)
                        }
               ; spaces
               ; d <- do{ string "LR"
                        ; return True
                        }
                      <|>
                      do{ string "RL"
                        ; return False
                        }
               ; spaces
               ; string "at"
               ; spaces
               ; w <- do{ string "e"
                        ; return []
                        }
                      <|>
                      sepBy1 (many1 digit >>= return . (flip (-) 1) . read) (char '.')
               ; spaces
               ; string "with"
               ; spaces
               ; char '{'
               ; rs <- sepBy (do{ char 'x'
                                ; n <- many1 digit
                                ; spaces
                                ; string "<-"
                                ; spaces
                                ; e <- parseWMExpr
                                ; return (read n, e)
                                })
                             (string ", ")
               ; char '}'
               ; spaces
               ; return (Appli r d w rs)
               } <?> "parseAppli"

parseWMProof :: Parser WMProof
parseWMProof = do{ string "Consider the following set of axioms:"
                 ; spaces
                 ; as <- many parseAxiom
                 ; spaces
                 ; string "This theorem holds true:"
                 ; spaces
                 ; parseTheoremHead
                 ; spaces
                 ; string "Proof:"
                 ; spaces
                 ; ls <- many parseLemma
                 ; spaces
                 ; t <- parseTheorem
                 ; return (WMProof as ls t)
                 } <?> "parseWMProof"

parseAxiom :: Parser WMEquation
parseAxiom = do{ string "Axiom "
               ; takeWhile1 isDigit
               ; string ": "
               ; spaces
               ; e <- parseWMEquation
               ; spaces
               ; return e
               } <?> "parseAxiom"

-- +----------------------------------------------------------------+
-- + 2. Term Reconstruction                                         +
-- +----------------------------------------------------------------+

data TypedExpr = TFun Int [TypedExpr] Term
               | TVar Text Term deriving (Show, Eq)

variablesMap :: TypedExpr -> Map Text Term
variablesMap = Map.fromList . flatten
  where
    flatten :: TypedExpr -> [(Text, Term)]
    flatten (TVar v t) = [(v, t)]
    flatten (TFun _ exprs _) = concatMap flatten exprs

variables :: TypedExpr -> [(Text, Term)]
variables = Map.toList . variablesMap

typeExpr :: (Functor m, Monad m) => Expr -> TCMT m (Maybe TypedExpr)
typeExpr (Var n) = return Nothing
typeExpr expr = Just <$> typeExpr' expr

typeExpr' :: (Functor m, Monad m) => Expr -> TCMT m TypedExpr
typeExpr' (Cnst _) =
  __ERROR__ "waldmeister" [] "output contains const"
typeExpr' (Con n exprs) = do
    ty <- infer (Unnamed (DB n "!"))
    tExprs <- typeExprList exprs (piToList ty)
    return (TFun n tExprs (piResult ty))
  where
    typeExprList :: (Functor m, Monad m) => [Expr] -> [Term] -> TCMT m [TypedExpr]
    typeExprList ((Var n):es) (t:ts) = (TVar n t :) <$> typeExprList es ts
    typeExprList (e:es) (t:ts) = typeExpr' e >>= \te -> (te :) <$> typeExprList es ts
    typeExprList [] t = return []

unTypeExpr :: TypedExpr -> Expr
unTypeExpr (TVar v _) = Var v
unTypeExpr (TFun n exprs _) = Con n (map unTypeExpr exprs)

piToList :: Term -> [Term]
piToList (Pi _ s t) = s : piToList (shift (-1) 0 t)
piToList t = [t]

piResult :: Term -> Term
piResult (Pi _ s t) = piResult (shift (-1) 0 t)
piResult t = t

piFromList :: [Term] -> Term
piFromList [t] = t
piFromList (s:ts) = Pi (Tag "_") s $ piFromList (map (shift 1 0) ts)

exprArity :: Expr -> [Text]
exprArity (Var v) = [v]
exprArity (Con n []) = []
exprArity (Con n exprs) = exprArity =<< exprs
exprArity (Cnst _) = []

typedExprArity :: TypedExpr -> [Text]
typedExprArity = exprArity . unTypeExpr

lemmaAllVars :: WMLemma -> [Text]
lemmaAllVars (Lemma _ steps) = nub (exprArity . snd =<< steps)

lemmaArity :: WMLemma -> [Text]
lemmaArity (Lemma (expr1, expr2) _) = nub (exprArity expr1 ++ exprArity expr2)

rcTypedExpr :: (Functor m, Monad m) => TypedExpr -> TCMT m (Term, Term)
rcTypedExpr expr = do
    ctx <- getCtx
    let t = lamBinders (typedExprArity expr) (rcTypedExprTerm expr)
        ty = piFromList (map snd (variables expr) ++ [rcTypedExprType expr])
    return (fixNames ctx t, fixNames ctx ty)

rcExprTerm :: Expr -> Term
rcExprTerm (Var n) = Named n
rcExprTerm (Cnst _) = Named "CONSTANT" -- This should cause the type checker to fail.
rcExprTerm (Con n exprs) =
    foldl App (Unnamed (DB n "!")) $ map rcExprTerm exprs

rcTypedExprTerm :: TypedExpr -> Term
rcTypedExprTerm = rcExprTerm . unTypeExpr

rcTypedExprType :: TypedExpr -> Term
rcTypedExprType (TVar _ ty) = ty
rcTypedExprType (TFun _ _ ty) = ty

wmEquationVariables :: (Functor m, Monad m) => WMEquation -> TCMT m [(Text, Term)]
wmEquationVariables (expr1, expr2) =
    Map.toList <$> liftM2 merge (typeExpr expr1) (typeExpr expr2)
  where
    vs = maybe Map.empty variablesMap
    merge x y = Map.union (vs x) (vs y)

rcWMEquation :: (Functor m, Monad m) => WMEquation -> TCMT m (Term, Term, Term, [(Text, Term)])
rcWMEquation wmEq@(expr1, expr2) = do
    ctx <- getCtx
    (Just texpr) <- liftM2 (<|>) (typeExpr expr1) (typeExpr expr2)
    vars <- wmEquationVariables wmEq
    return (rcTypedExprType texpr, rcExprTerm expr1, rcExprTerm expr2, vars)

rcAxiom :: WMEquation -> TCMT IO Term
rcAxiom wmEq = do
    (ty, t1, t2, vars) <- rcWMEquation wmEq
    ctx@(Ctx bs _) <- getCtx
    let ax = piBinders vars (Id ty t1 t2)
    axV <- findAxiom 0 ax
    return $ fixNames ctx axV --OK
  where
    findAxiom n ax = do
        ty <- tnf =<< getUnnamedVar n
        if ax == ty then return (Unnamed (DB n "!")) else findAxiom (n + 1) ax

subExpr :: [Int] -> Expr -> Expr
subExpr [] e = e
subExpr (n:ns) (Con _ exprs) = subExpr ns (exprs !! n)

subExprRemove :: Text -> [Int] -> Expr -> Term
subExprRemove name [] t = Named name
subExprRemove name (n:ns) (Con m exprs) = foldl App (Unnamed (DB m "!")) $ h ++ l ++ t
  where
    h = map rcExprTerm $ Prelude.take n exprs
    l = [subExprRemove name ns (exprs !! n)]
    t = map rcExprTerm $ drop (n + 1) exprs

rcCong :: Expr -> Expr -> [Int] -> Term -> (Expr, Expr, Term -> Term)
rcCong from to [] ty = (from, to, id)
rcCong from to location ty =
    (from', to', \t -> foldl App (Named "cong") $ [ty, ty, rcExprTerm from', rcExprTerm to', congF, t])
  where
    congF = lamBinders ["rc-cong-var"] $ subExprRemove "rc-cong-var" location from
    from' = subExpr location from
    to' = subExpr location to

rcSym :: Expr -> Expr -> Term -> Term -> Term
rcSym from to ty t = foldl App (Named "sym") $ [ty, rcExprTerm to, rcExprTerm from, t]

rcAppli :: [Term] -> [Term] -> Appli -> Expr -> Expr -> Term ->  Term
rcAppli axs lemmas (Appli rule lr loc args) from to ty =
    let (from', to', congF) = rcCong from to loc ty
        symF = if lr then id else rcSym from' to' ty
        argTerms = map (snd . second rcExprTerm) args
        appli = case rule of
                  Ax n -> foldl App (axs !! (n - 1)) (reverse argTerms)
                  Lem n -> foldl App (lemmas !! (n - 1)) (reverse argTerms)
    in congF . symF $ appli

rcRewrites :: [Term] -> [Term] -> [Rewrite] -> Expr -> Expr -> Term -> Term
rcRewrites axs lemmas [(appli,to)] from fto ty =
    rcAppli axs lemmas appli from fto ty

rcRewrites axs lemmas ((appli, to):rewrites) from fto ty =
    let rewrite = rcAppli axs lemmas appli from to ty
        rewrites' = rcRewrites axs lemmas rewrites to fto ty
    in foldl App (Named "trans")
           [ty, rcExprTerm from, rcExprTerm to, rcExprTerm fto, rewrite, rewrites']

rcWMLemma :: [Term] -> [Term] -> WMLemma -> TCMT IO (Term, Term)
rcWMLemma axs lemmas lem@(Lemma proves@(from, to) rewrites) = do
    (ty, t1, t2, args) <- rcWMEquation proves

    let existVars = lemmaAllVars lem \\ lemmaArity lem
    ctx <- getCtx

    (n : _) <- getUnnamedVarByType (Sort Star)
    ms <- getUnnamedVarByType (fixNames ctx (Unnamed (DB n "!")))

    let existSubsts = map (\m -> fixNames ctx (Unnamed (DB m "!"))) ms

    let term = fixNames ctx . lamBinders (map fst args) . cata 0 (fixExist existVars (lemmaArity lem) existSubsts) $
                   rcRewrites axs lemmas rewrites from to ty
        lemTy = fixNames ctx $ piBinders args (Id ty t1 t2)
    return (term, lemTy) -- FIXME
  where
    fixExist vs [] (r : _) _ (Named v) | v `elem` vs = r
    fixExist vs r1 _       _ (Named v) | v `elem` vs = (Named (head r1))
    fixExist _  _  _       _ t                       = t

gensym :: IO Text
gensym = T.pack . map chr <$> replicateM 10 (randomRIO (97, 122))

pullout :: Term -> Term -> TCMT IO Term
pullout t ty = do
    ctx@(Ctx bs fs) <- getCtx
    let pt = flip (foldr Lam) (map fst (reverse bs)) t
        pty = flip (foldr (uncurry Pi)) (reverse bs) ty
    --typecheck pt pty
    random <- liftIO $ gensym
    alpha <- T.pack <$> Lang.TypeChecker.Monad.count
    let name = T.concat ["wmLemma", alpha, random]
    putCtx (Ctx bs (OMap.insert name (pt, pty) fs))
    let vars = map (Unnamed . uncurry DB . second toText) $ reverse (zip [0..] (map fst bs))
    return (foldl App (Named name) vars)

rcWMProof :: WMProof -> TCMT IO Term
rcWMProof proof = do
    axs <- mapM rcAxiom (axioms proof)
    liftIO $ putStr "Processing Lemmas: "
    lems <- rcLemmas 0 axs [] (lemmas proof)
    liftIO $ putChar '\n'
    fst <$> rcWMLemma axs lems (theorem proof)
  where
    rcLemmas :: Int -> [Term] -> [Term] -> [WMLemma] -> TCMT IO [Term]
    rcLemmas n axs defined [] = return defined
    rcLemmas n axs defined (lem : lems) = do
        (t, ty) <- rcWMLemma axs defined lem
        liftIO $ putStr (show (n + 1) ++ " ")
        liftIO $ hFlush stdout
        lemmaTerm <- pullout t ty
        rcLemmas (n + 1) axs (defined ++ [lemmaTerm]) lems



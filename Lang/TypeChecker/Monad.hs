{-# LANGUAGE OverloadedStrings, CPP, GeneralizedNewtypeDeriving, ExistentialQuantification #-}

module Lang.TypeChecker.Monad where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State (StateT, runStateT)
import qualified Control.Monad.Trans.State as State

import Data.Char (toUpper, chr)
import Data.List (intersperse)
import qualified Data.Attoparsec.Text as Atto
import Data.Map (Map)
import Data.String (IsString (..))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Lang.Util.Counter
import Lang.Util.EitherT
import Lang.Util.OMap (OMap)
import qualified Lang.Util.OMap as OMap

import Lang.Error
import Lang.PrettyPrint
import Lang.Term
import qualified Lang.Term.Parser as Parser

#define __ERROR__ typeError "Lang.TypeChecker.Monad" __LINE__

-- The typechecking monad transformer (TCMT).
newtype TCMT m a = TCMT
    { unTCMT :: EitherT TypeError
                        (StateT (TCMState m) m)
                        a
    } deriving (Functor, Applicative, Monad, MonadIO)

data TCMState m = TCMState { tcmDepth :: Int
                           , tcmCtx :: Ctx
                           , tcmTCRules :: [TCRule m]
                           , tcmIRules :: [IRule m]
                           , tcmMetas :: [MetaContinuation]
                           , tcmLog :: [LogEntry]
                           , tcmCounter :: Counter
                           }

data MetaContinuation = MC Ctx Int Term deriving (Show)

instance PrettyPrint MetaContinuation where
    pretty sch (MC _ n term) = T.concat [T.pack (show n), " / ", pretty sch term]

data LogEntry = LTrace Int Term Term (Either TypeError Text)
              | LText Text
              deriving (Show)

get :: (Monad m) => TCMT m (TCMState m)
get = TCMT (lift State.get)

put :: (Monad m) => TCMState m -> TCMT m ()
put = TCMT . lift . State.put

count :: (Monad m) => TCMT m String
count = do
    state <- get
    let (Counter n) = tcmCounter state
    put $ state { tcmCounter = Counter (n + 1) }
    return $ alphas !! n
  where alphas = concatMap (\a -> map (\b -> [a,b]) ['a'..'z']) ['a'..'z']

data Msg = forall m. PrettyPrint m => Msg m

instance PrettyPrint Msg where
    pretty sch (Msg m) = pretty sch m

instance IsString Msg where
    fromString = Msg . T.pack . map toUpper

msg :: (Monad m) => [Msg] -> TCMT m ()
msg xs = tell (LText (T.concat (intersperse " " (map (pretty debugScheme) xs))))

tell :: (Monad m) => LogEntry -> TCMT m ()
tell entry = do
    state <- get
    put $ state { tcmLog = entry : tcmLog state }

hasType :: (Functor m, Monad m) => Term -> Term -> TCMT m ()
a `hasType` b = do
    result <- typecheck a b
    case result of
      Just rule -> return ()
      Nothing -> __ERROR__ "hasType" [("a", a), ("b", b)]
                 "No rule can be used to check that {a}\n is of type\n{b}"

depth :: Monad m => (Int -> Int) -> TCMT m ()
depth f = do
    s <- get
    put $ s { tcmDepth = f (tcmDepth s) }

data TCRule m = TCR { ruleName :: Text
                    , rule :: Term -> Term -> TCMT m Bool
                    }

typecheck :: (Functor m, Monad m) => Term -> Term -> TCMT m (Maybe Text)
typecheck t1 t2 = do
    tRules <- tcmTCRules <$> get
    depth (+ 1)
    foldM tryRule Nothing tRules
  where
    tryRule (Just name) _ = return (Just name)
    tryRule Nothing (TCR name rule) = do
        r <- rule t1 t2
        return $ if r then Just name else Nothing


data IRule m = IR { inferRuleName :: Text
                  , inferRule :: Term -> TCMT m (Maybe Term)
                  }

infer :: (Functor m, Monad m)  => Term -> TCMT m Term
infer t | inf t = do
    iRules <- tcmIRules <$> get
    r <- foldM tryRule Nothing iRules
    case r of
      (Just t) -> return t
      Nothing -> __ERROR__ "infer" [("t", t)]
                 "no rule could be applied to infer the type of\n{t}"
  where tryRule (Just t) _ = return (Just t)
        tryRule Nothing (IR name rule) = rule t

infer t = __ERROR__ "infer" [("t", t)] "{t}\nis not inferable"

isSort :: (Functor m, Monad m) => Term -> TCMT m Sort
isSort (Sort s) = return s
isSort t = __ERROR__ "isSort" [("t", t)] "{t}\nis not a sort"

-- | 'validType' checks if a value has type '*' or '□n'.
validType :: (Functor m, Monad m) => Term -> TCMT m Sort
validType t | inf t = do
    s <- infer t
    case s of
      (Sort n) -> return n
      otherwise -> __ERROR__ "validType" [("t", t)] "{t}\nis an invalid type"
validType t = do
    t `hasType` Sort Star
    return Star

runTCMT :: Monad m => TCMState m -> TCMT m a -> m (Either TypeError a, TCMState m)
runTCMT state (TCMT action) = runStateT (runEitherT action) state

tnf :: (Functor m, Monad m) => Term -> TCMT m Term
tnf term = do
    ctx <- getCtx
    case nf ctx term of
      Left name   -> __ERROR__ "tnf" [] (T.concat ["variable ", name, " not defined"])
      Right term' -> return term'

-- +----------------------------------------------------------------+
-- + 1. Contexts                                                    +
-- +----------------------------------------------------------------+

getCtx :: (Functor m, Monad m) => TCMT m Ctx
getCtx = tcmCtx <$> get

putCtx :: Monad m => Ctx -> TCMT m ()
putCtx ctx = do
    tcmState <- get
    put $ tcmState {tcmCtx = ctx}

addNamedVar :: Text -> Term -> Term -> Ctx -> Ctx
addNamedVar name term ty (Ctx bs fs) = Ctx bs $ OMap.insert name (term, ty) fs

getUnnamedVar :: (Functor m, Monad m) => Int -> TCMT m Term
getUnnamedVar n = do
    ctx <- getCtx
    getUnnamedVar' n ctx
  where
    getUnnamedVar' 0 (Ctx ((_, b) : bs) fs) = return $ shift (n + 1) 0 b -- cata 0 (updateVars (n + 1)) b
    getUnnamedVar' m (Ctx (_ : bs) fs) = getUnnamedVar' (m - 1) $ Ctx bs fs
    getUnnamedVar' _ (Ctx [] _) = -- CPP doesn't like it if __ERROR__ is on this line
        __ERROR__ "getUnnamedVar" [] "index too high"

getUnnamedVarByType :: (Functor m, Monad m) => Term -> TCMT m [Int]
getUnnamedVarByType ty = do
    ctx <- getCtx
    getUnnamedVarByType' 0 (shift (-1) 0 ty) ctx
  where
    getUnnamedVarByType' n ty (Ctx [] _) = return []
    getUnnamedVarByType' n ty (Ctx ((_, v) : vs) ns) | v == ty = (n :) <$> getUnnamedVarByType' (n + 1) (shift (-1) 0 ty) (Ctx vs ns)
    getUnnamedVarByType' n ty (Ctx (_ : vs) ns) = getUnnamedVarByType' (n + 1) (shift (-1) 0 ty) (Ctx vs ns)

{-
    updateVars m o (Unnamed (DB n name)) | n + 1 > o = Unnamed (DB (n + m) name)
    updateVars _ _ x = x
-}

getNamedVar :: (Functor m, Monad m) => Text -> TCMT m (Term, Term)
getNamedVar name = do
    (Ctx _ fs) <-  getCtx
    case OMap.lookup name fs of
      Nothing -> __ERROR__ "getNamedVar" [] (T.concat ["variable ", name, " not defined"])
      Just t  -> return t

withUnnamedVar :: (Functor m, Monad m) => Tag -> Term -> TCMT m a -> TCMT m a
withUnnamedVar name b action = do
    ctx@(Ctx bs fs) <- getCtx
    putCtx (Ctx ((name, b):bs) fs)
    result <- action
    putCtx ctx
    return result

substCtx :: Int -> Term -> Ctx -> Ctx
substCtx n t (Ctx bs fs) = Ctx (substCtx' n t bs) fs

substCtx' :: Int -> Term -> [(Tag, Term)] -> [(Tag, Term)]
substCtx' 1 t ((tag, b):bs) = (tag, subst 0 (shift (-1) 0 t) b) : bs
substCtx' n t ((tag, b):bs) =
    (tag, subst (n - 1) (shift (-1) 0 t) b) : substCtx' (n - 1) (shift (-1) 0 t) bs

withSubstCtx :: (Functor m, Monad m) => Int -> Term -> TCMT m a -> TCMT m a
withSubstCtx i t action = do
    ctx <- getCtx
    putCtx (substCtx i t ctx)
    result <- action
    putCtx ctx
    return result

-- +----------------------------------------------------------------+
-- + 2. Type Errors                                                 +
-- +----------------------------------------------------------------+

data TypeError = TypeError
    { errDesc :: Text
    , errTerms :: Map Text Term
    , errCtx :: Ctx
    , errFunction :: Text
    , errFile :: String
    , errLine :: Integer
    } deriving (Show)

typeError :: (Functor m, Monad m)
          => String -> Integer -- ^ should be supplied with CPP
          -> Text -> [(Text, Term)] -> Text -> TCMT m a
typeError file line fun terms desc = TCMT $ do
    ctx <- unTCMT getCtx
    err TypeError { errDesc = desc
                  , errTerms = Map.fromList terms
                  , errFunction = fun
                  , errCtx = ctx
                  , errFile = file
                  , errLine = line
                  }

instance PrettyPrint TypeError where
    pretty sch terr =
        T.concat [ outputDesc sch (errDesc terr) (errTerms terr)
                 , "Raised by ", errFunction terr
                 , " at line ", T.pack (show (errLine terr))
                 , " in file ", T.pack (errFile terr)
                 ]

outputDesc :: ColorScheme -> Text -> Map Text Term -> Text
outputDesc sch desc terms =
    case T.break (== '{') desc of
      (t, "") -> T.concat [ t, "\n"]
      (t, t') ->
          let (var, t'') = T.break (== '}') t'
          in T.concat
                 [ t
                 , pretty sch
                   (Map.findWithDefault (error (show var)) (T.tail var) terms)
                 , outputDesc sch (T.tail t'') terms
                 ]

-- other utilities...

assign :: (Functor m, Monad m) => Term -> TCMT m Term
assign t = do
    ctx <- getCtx
    return $ assignVars ctx t

parseTerm :: (Functor m, Monad m) => Text -> TCMT m Term
parseTerm t = do
    ctx <- getCtx
    let res = runError defaultScheme $ Parser.parseTerm ctx [] t
    case res of
      Right term -> return term
      Left e -> __ERROR__ "parseTerm" [] e


{-# LANGUAGE OverloadedStrings, PatternGuards, GeneralizedNewtypeDeriving #-}

module Lang.TopLevel.Monad
    ( -- * Command Impementations
      Opts
    , CommandImpl (..)
    , Verbosity (..)
    , commandsFromList, optLookup, optLookupDefault
      -- * TopLevel Monad
    , TLState (..)
    , defaultState
    , TopLevel
    , runTopLevel
      -- ** Accessors
    , getCtx, getColorScheme, getCounter, getCurrentTheorem
    , getScriptVars, getMetas, getVerbosity
    , lookupCommandImpl
      -- ** Pretty printing in the TopLevel monad.
    , newline, prettyPrint, colorBanner, isDefinedMsg, setVerbosity
    , putStrLnV, putStrLn, putCharV, putChar, putStrV, putStr
      -- ** Error handling in the TopLevel monad.
    , end, errorMsg, try, recover, onFail, ignoreFail, formatTypeError
      -- * State modification functions
      -- ** Named Variables
    , addNamedVar, removeNamedVar
      -- ** Script Variables
    , addScriptVar, removeScriptVar, modifyScriptVar, getScriptVar, lookupScriptVar
    , equalScriptVar, incScriptVar
      -- ** Current Theorem
    , setCurrentTheorem, endCurrentTheorem, setSorry
      -- ** Metas
    , addMetas, modifyMetas, solveMeta, clearMetas
    , getCurrentMeta, getCurrentEquation
    , getSolvedMetas, modifySolvedMetas, addSolvedMeta, clearSolvedMetas
      -- ** Counter
    , withCounter
      -- * Type checking, parsing and normalization.
    , simpleCheck, simpleInfer
    , tlnf
    , parseTerm
      -- * Utilities
    , identToText
      -- * Running Commands
    , runCommand
    ) where

import qualified Prelude as Prelude
import Prelude hiding (putStrLn, putChar, putStr)

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State as State
import Control.Monad.IO.Class

import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Exit

import Lang.Util.Counter
import Lang.Util.UndoStateT

import qualified Lang.PrettyPrint as PP
import Lang.PrettyPrint hiding (colorBanner, prettyPrint)
import Lang.Term
import qualified Lang.Term.Parser as Parser
import Lang.Error
import Lang.TopLevel.Parser
import Lang.TypeChecker.Monad hiding (parseTerm, putCtx, getCtx, addNamedVar)
import qualified Lang.TypeChecker.TypeChecker as TypeChecker

-- +----------------------------------------------------------------+
-- + TopLevel Monad                                                 +
-- +----------------------------------------------------------------+

type Opts = Map Text [Expr]

data Verbosity = Silent | Normal | Verbose deriving (Eq, Ord, Enum)

data CommandImpl = Impl { implName :: Text
                        , implFun :: [Expr] -> (Map Text [Expr]) -> TopLevel ()
                        , implDoc :: Text
                        }

optLookup :: Text -> Map Text a -> TopLevel a
optLookup k opts
  | (Just x) <- Map.lookup k opts = return x
  | otherwise                     = errorMsg (T.concat ["Keyword option ", k, " is missing"])

optLookupDefault :: a -> Text -> Map Text a -> TopLevel a
optLookupDefault d k opts
  | (Just x) <- Map.lookup k opts = return x
  | otherwise                     = return d

commandsFromList :: [CommandImpl] -> Map Text CommandImpl
commandsFromList = Map.fromList . map (implName &&& id)

data TLState = TL { tlColorScheme :: ColorScheme
                  , tlPrelude :: FilePath
                  , tlCtx :: Ctx
                  , tlCurrentTheorem :: Maybe (Text, Term)
                  , tlMetas :: [MetaContinuation]
                  , tlSolvedMetas :: Map Int Term
                  , tlCounter :: Counter
                  , tlScriptVars :: Map Text Text
                  , tlCommandImpls :: Map Text CommandImpl
                  , tlSorry :: Bool
                  , tlVerbosity :: Verbosity
                  }

defaultState = TL { tlColorScheme = defaultScheme
                  , tlPrelude = "/home/alasdair/prelude.prv"
                  , tlCtx = emptyCtx
                  , tlCurrentTheorem = Nothing
                  , tlMetas = []
                  , tlSolvedMetas = Map.empty
                  , tlCounter = Counter 0
                  , tlScriptVars = Map.empty
                  , tlCommandImpls = Map.empty
                  , tlSorry = False
                  , tlVerbosity = Normal
                  }

type TopLevel a = UndoStateT TLState (MaybeT IO) a

runTopLevel :: TopLevel a -> TLState -> IO (Maybe a)
runTopLevel tl state =
    runMaybeT $ evalStateT (unUS tl) ([], state)

-- State accessors.

getCtx :: TopLevel Ctx
getCtx = tlCtx <$> getState

getColorScheme :: TopLevel ColorScheme
getColorScheme = tlColorScheme <$> getState

getCounter :: TopLevel Counter
getCounter = tlCounter <$> getState

getCurrentTheorem :: TopLevel (Maybe (Text, Term))
getCurrentTheorem = tlCurrentTheorem <$> getState

getScriptVars :: TopLevel (Map Text Text)
getScriptVars = tlScriptVars <$> getState

getMetas :: TopLevel [MetaContinuation]
getMetas = tlMetas <$> getState

getVerbosity :: TopLevel Verbosity
getVerbosity = tlVerbosity <$> getState

getSorry :: TopLevel Bool
getSorry = tlSorry <$> getState

lookupCommandImpl :: Text -> TopLevel (Maybe CommandImpl)
lookupCommandImpl cmd = do
    commands <- tlCommandImpls <$> getState
    return $ Map.lookup cmd commands

-- +----------------------------------------------------------------+
-- + Pretty printing in the TopLevel monad                          +
-- +----------------------------------------------------------------+

putStrLn :: Text -> TopLevel ()
putStrLn = putStrLnV Normal

putStrLnV :: Enum v => v -> Text -> TopLevel ()
putStrLnV v text = do
    verbosity <- fromEnum <$> getVerbosity
    if fromEnum v <= verbosity
    then liftIO (T.putStrLn text)
    else liftIO (T.appendFile "/tmp/proveit-err" (T.append text "\n"))

putStr :: Text -> TopLevel ()
putStr = putStrV Normal

putStrV :: Enum v => v -> Text -> TopLevel ()
putStrV v text = do
    verbosity <- fromEnum <$> getVerbosity
    if fromEnum v <= verbosity
    then liftIO (T.putStr text)
    else liftIO (T.appendFile "/tmp/proveit-err" text)

putCharV :: Enum v => v -> Char -> TopLevel ()
putCharV v c = do
    verbosity <- fromEnum <$> getVerbosity
    if fromEnum v <= verbosity
    then liftIO (Prelude.putChar c)
    else liftIO (appendFile "/tmp/proveit-err" [c])

putChar :: Char -> TopLevel ()
putChar = putCharV Normal

newline :: TopLevel ()
newline = putChar '\n'

prettyPrint :: PrettyPrint p => p -> TopLevel ()
prettyPrint p = do
    verbosity <- getVerbosity
    sch <- getColorScheme
    case verbosity of
      Silent    -> liftIO $ T.appendFile "/tmp/proveit-err" (pretty sch p)
      otherwise -> liftIO $ PP.prettyPrint sch p

colorBanner :: (ColorScheme -> Text) -> Text -> TopLevel ()
colorBanner color msg = do
    sch <- getColorScheme
    putStrLn (PP.colorBanner sch color msg)

isDefinedMsg :: Text -> TopLevel ()
isDefinedMsg name = do
    sch <- getColorScheme
    putStrLn (T.concat [namedColor sch, name, reset sch, " defined"])

-- +----------------------------------------------------------------+
-- + Error Handling in the TopLevel monad                           +
-- +----------------------------------------------------------------+

end :: TopLevel a
end = undo >> lift mzero

errorMsg :: Text -> TopLevel a
errorMsg msg = do
    sch <- getColorScheme
    putChar '\n'
    colorBanner errorColor "COMMAND ERROR"
    putStrLn msg
    end

-- | Lift an error action into the TopLevel monad.
try :: Error a -> TopLevel a
try err = do
    sch <- getColorScheme
    case runError sch err of
      (Right a) -> return a
      (Left msg) -> errorMsg msg

-- | Recover if an error occured.
recover :: TopLevel a -> TopLevel (Maybe a)
recover tl = US . StateT $ \s -> MaybeT $ do
    res <- runMaybeT (runStateT (unUS tl) s)
    case res of
      Just (a, s') -> return (Just (Just a, s'))
      Nothing -> return (Just (Nothing, s))

onFail :: TopLevel a -> TopLevel a -> TopLevel a
onFail act tl = do
    res <- recover tl
    case res of
       Just x -> return x
       Nothing -> act

ignoreFail :: TopLevel () -> TopLevel ()
ignoreFail tl = do
    res <- recover tl
    return ()

formatTypeError :: TypeError -> [LogEntry] -> TopLevel a
formatTypeError err log = do
    sch <- getColorScheme

    newline >> colorBanner errorColor "TYPE ERROR" >> newline

    prettyPrint err

    when (not (null (filter logFilter log))) $ do
        putStrLn "\nbecause:"
        prettyPrint (head (filter logFilter log))

    putStrLn "\nCtx:"
    mapM_ (\(tag, ty) -> putStrLn $ T.concat [pretty sch tag, " : ", pretty sch ty]) (unnamed (errCtx err))

    let messages = filter msgFilter log
    when (not (null messages)) $ putStrLn "\nMessages:" >> mapM_ prettyPrint messages

    newline
    end

logFilter :: LogEntry -> Bool
logFilter (LTrace _ _ _ (Left _)) = True
logFilter _ = False

msgFilter :: LogEntry -> Bool
msgFilter (LText _) = True
msgFilter _ = False

-- +----------------------------------------------------------------+
-- + State modification functions                                   +
-- +----------------------------------------------------------------+

modifyCtx :: (Ctx -> Ctx) -> (Ctx -> Ctx) -> TopLevel ()
modifyCtx change undo = modifyState (modifyCtx' change) (modifyCtx' undo)
  where
    modifyCtx' f state = state { tlCtx = f (tlCtx state) }

withNamed :: (Map Text (Term, Term) -> Map Text (Term, Term)) -> Ctx -> Ctx
withNamed f (Ctx u n) = Ctx u (f n)

addNamedVar :: Text -> Term -> Term -> TopLevel ()
addNamedVar name t ty = do
    (Ctx _ named) <- getCtx
    if (Map.member name named)
    then errorMsg (T.append name " is already defined")
    else modifyCtx (withNamed (Map.insert name (t, ty))) (withNamed (Map.delete name))

removeNamedVar :: Text -> TopLevel ()
removeNamedVar name = do
    (Ctx _ named) <- getCtx
    case Map.lookup name named of
      Nothing -> errorMsg (T.append name " is not defined")
      Just (t, ty) ->
          modifyCtx (withNamed (Map.delete name)) (withNamed (Map.insert name (t, ty)))

modifyScriptVars :: (Map Text Text -> Map Text Text) -> (Map Text Text -> Map Text Text) -> TopLevel ()
modifyScriptVars change undo = modifyState (modifyTV change) (modifyTV undo)
  where
    modifyTV f state = state { tlScriptVars = f (tlScriptVars state) }

addScriptVar :: Text -> Text -> TopLevel ()
addScriptVar name term = do
    vars <- getScriptVars
    if Map.member name vars
    then errorMsg (T.concat ["variable ", name, " is already defined"])
    else modifyScriptVars (Map.insert name term) (Map.delete name)

removeScriptVar :: Text -> TopLevel ()
removeScriptVar name = do
    vars <- getScriptVars
    case Map.lookup name vars of
      Nothing -> return ()
      Just term -> modifyScriptVars (Map.delete name) (Map.insert name term)

modifyScriptVar :: Text -> Text -> TopLevel ()
modifyScriptVar name term = do
    removeScriptVar name
    addScriptVar name term

getScriptVar :: Text -> TopLevel Text
getScriptVar name = do
    vars <- getScriptVars
    maybe (errorMsg msg) return (Map.lookup name vars)
  where
    msg = T.concat ["cannot lookup variable: ", name, " is not defined"]

lookupScriptVar :: Text -> TopLevel (Maybe Text)
lookupScriptVar name = do
    vars <- getScriptVars
    return (Map.lookup name vars)

equalScriptVar :: Text -> Text -> TopLevel Bool
equalScriptVar name eq = do
    var <- lookupScriptVar name
    case var of
      Just val -> return $ val == eq
      Nothing  -> return False

incScriptVar :: Text -> TopLevel ()
incScriptVar name = do
    var <- lookupScriptVar name
    case var of
      Just val -> modifyScriptVar name (T.pack (show (1 + (read (T.unpack val)))))
      Nothing  -> error "variable does not exist"

modCT :: Maybe (Text, Term) -> TLState -> TLState
modCT ct state = state { tlCurrentTheorem = ct }

setCurrentTheorem :: Text -> Term -> TopLevel ()
setCurrentTheorem newName newTerm = do
    currentTheorem <- getCurrentTheorem
    case currentTheorem of
      Just (name, _) -> errorMsg (T.append "Already defining theorem " name)
      Nothing        -> modifyState (modCT (Just (newName, newTerm))) (modCT currentTheorem)

endCurrentTheorem :: TopLevel ()
endCurrentTheorem = do
    currentTheorem <- getCurrentTheorem
    case currentTheorem of
      Nothing         -> errorMsg "No theorem being defined"
      Just (name, ty) -> modifyState (modCT Nothing) (modCT currentTheorem)

addMetas :: [MetaContinuation] -> TopLevel ()
addMetas metas = modifyState (modifyMetas (metas ++)) (modifyMetas (drop (length metas)))

modifyMetas :: ([MetaContinuation] -> [MetaContinuation]) -> TLState -> TLState
modifyMetas f state = state { tlMetas = f (tlMetas state) }

solveMeta :: Term -> TopLevel ()
solveMeta solution = do
    (meta@(MC _ n _) : _) <- tlMetas <$> getState
    modifyState (modifyMetas tail) (modifyMetas (meta :))
    addSolvedMeta n solution

clearMetas :: TopLevel ()
clearMetas = do
    metas <- getMetas
    modifyState (modifyMetas (const [])) (modifyMetas (const metas))

getCurrentMeta :: TopLevel MetaContinuation
getCurrentMeta = do
    metas <- tlMetas <$> getState
    case metas of
      (meta : _) -> return meta
      otherwise -> errorMsg "There are no metas!"

getCurrentEquation :: TopLevel (Term, Term, Term)
getCurrentEquation = do
    metas <- tlMetas <$> getState
    case metas of
      ((MC _ _ (Id ty t1 t2)) : _) -> return (ty, t1, t2)
      otherwise -> errorMsg "There are no metas!"

getSolvedMetas :: TopLevel (Map Int Term)
getSolvedMetas = tlSolvedMetas <$> getState

modifySolvedMetas :: (Map Int Term -> Map Int Term) -> (Map Int Term -> Map Int Term) -> TopLevel ()
modifySolvedMetas change undo = modifyState (modSM change) (modSM undo)
  where
    modSM f state = state { tlSolvedMetas = f (tlSolvedMetas state) }

addSolvedMeta :: Int -> Term -> TopLevel ()
addSolvedMeta n t = do
    solved <- getSolvedMetas
    if (Map.member n solved)
    then putStrLn "FATAL: duplicate meta id" >> liftIO exitFailure
    else modifySolvedMetas (Map.insert n t) (Map.delete n)

clearSolvedMetas :: TopLevel ()
clearSolvedMetas = do
    solved <- getSolvedMetas
    modifySolvedMetas (const Map.empty) (const solved)

setSorry :: Bool -> TopLevel ()
setSorry b = modifyState (\state -> state { tlSorry = b }) (\state -> state { tlSorry = not b })

setVerbosity :: Verbosity -> TopLevel ()
setVerbosity v = modifyState (\state -> state { tlVerbosity = v }) (\state -> state { tlVerbosity = v })

withCounter :: (a -> State Counter a) -> a -> TopLevel a
withCounter f x = do
    counter <- getCounter
    let (x', counter') = runState (f x) counter
    modifyState (setCounter counter') (setCounter counter)
    return x'
  where
    setCounter x state = state { tlCounter = x }

-- +----------------------------------------------------------------+
-- + Type checking, normalization and parsing.                      +
-- +----------------------------------------------------------------+

parseTerm :: Ctx -> Text -> TopLevel Term
parseTerm ctx termText = do
    term <- try $ Parser.parseTerm ctx termText
    withCounter numberMetas term

tlnf :: Ctx -> Term -> TopLevel Term
tlnf ctx term = case nf ctx term of
         Left name  -> errorMsg (T.concat [name, " is not defined"])
         Right term -> return term

simpleCheck :: Ctx -> Term -> Term -> TopLevel ()
simpleCheck ctx t ty =
    case TypeChecker.simpleCheck ctx t ty of
      Just err -> formatTypeError err []
      Nothing  -> return ()

simpleInfer :: Ctx -> Term -> TopLevel Term
simpleInfer ctx t =
    case TypeChecker.simpleInfer ctx t of
      Left err -> formatTypeError err []
      Right ty -> return ty

-- Utils

identToText :: Expr -> TopLevel Text
identToText (ExprIdent t) = return t
identToText _ = errorMsg "invalid identifier"

runCommand :: Command -> TopLevel ()
runCommand command = do
  state <- getState
  let sch   = tlColorScheme state
      ctx   = tlCtx state
      sorry = tlSorry state

  case command of
    CmdExpr "sorry" [] opts | Map.null opts -> setSorry True
    CmdExpr "qed" _ _ | sorry -> do
        setSorry False
        incScriptVar "qedsorry"
        putStrLn "sqed"
        endCurrentTheorem
        clearMetas
        clearSolvedMetas
    CmdExpr name _ _ | name /= "qed" && sorry -> return ()

    CmdRefl | sorry -> return ()
    CmdRefl | not sorry -> do runCommand (CmdTerm "refl")

    CmdFun name ty t -> do
        t' <- parseTerm ctx t
        ty' <- parseTerm ctx ty
        simpleCheck ctx t' ty'
        addNamedVar name t' ty'
        isDefinedMsg name

    CmdTheorem name theoremText -> do
        theorem <- parseTerm ctx theoremText
        meta <- parseTerm ctx "?"
        setCurrentTheorem name theorem

        let tcmState = TypeChecker.defaultState { tcmCtx = ctx } :: TCMState Identity
            (result, tcmState') = runIdentity $ runTCMT tcmState (meta `hasType` theorem)

        case result of
          Left err -> formatTypeError err (tcmLog tcmState')
          Right () -> addMetas (tcmMetas tcmState')

    CmdTerm termText | sorry -> return ()
    CmdTerm termText | not sorry -> do
        metas <- tlMetas <$> getState
        case metas of
          [] -> errorMsg "No metas to solve!"
          (MC ctx n ty) : _ -> do
              term <- parseTerm ctx termText

              let tcmState = TypeChecker.defaultState { tcmCtx = ctx } :: TCMState Identity
                  (result, tcmState') = runIdentity $ runTCMT tcmState (term `hasType` ty)

              case result of
                Left err -> formatTypeError err (tcmLog tcmState')
                Right () -> solveMeta term >> addMetas (tcmMetas tcmState')

    CmdExpr name args opts -> do
        cmd <- lookupCommandImpl name
        case cmd of
          Just cmd' -> implFun cmd' args opts
          Nothing -> if (null args && Map.null opts)
                     then runCommand (CmdTerm name)
                     else errorMsg "Command not recognised!"

{-# LANGUAGE OverloadedStrings #-}
module Lang.TopLevel.Command.Base (baseCommands) where

import Prelude hiding (putStrLn, putChar, putStr)

import Lang.Util.OMap (OMap)
import qualified Lang.Util.OMap as OMap
import Lang.Util.UndoStateT
import Lang.Term
import Lang.PrettyPrint hiding (prettyPrint)
import Lang.TopLevel.Monad
import Lang.TopLevel.Parser

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.FilePath
import System.Directory

import System.Exit (exitSuccess)

baseCommands :: Map Text CommandImpl
baseCommands = commandsFromList $
    [ implUndo, implDefer, implInfer, implDescribe, implQuit
    , implDefined, implForget, implQed, implIntro, implNormalize
    , implHelp, implCommands, implAgda
    ]

implUndo :: CommandImpl
implUndo = Impl { implName = "undo"
                , implFun  = \_ _ -> undo >> undo
                , implDoc  = "Undoes the action performed by the preceding command."
                }

defer :: [a] -> [a]
defer []     = []
defer (x:xs) = xs ++ [x]

undefer :: [a] -> [a]
undefer [] = []
undefer xs = last xs : init xs

implDefer :: CommandImpl
implDefer = Impl { implName = "defer"
                 , implFun  = \_ _ -> modifyState (modifyMetas defer) (modifyMetas undefer)
                 , implDoc  = "Make the current goal the last goal."
                 }

infer :: [Expr] -> Opts -> TopLevel ()
infer [(ExprIdent t)] opts = infer [(ExprTerm t)] opts
infer [(ExprTerm t)] _ = do
  ctx <- getCtx
  t' <- parseTerm ctx t
  prettyPrint =<< simpleInfer ctx t'

implInfer :: CommandImpl
implInfer = Impl { implName = "infer"
                 , implFun  = infer
                 , implDoc  = "Infer the type of a term or identifier."
                 }

describe :: [Expr] -> Opts -> TopLevel ()
describe [(ExprIdent ident)] _ = do
    ctx <- getCtx
    sch <- getColorScheme

    case OMap.lookup ident (named ctx) of
      Nothing -> errorMsg (T.append ident " is not defined")
      Just (t, ty) -> do
          putStr $ withColor sch namedColor ident
          putStr " : "
          putStrLn $ pretty sch ty

          putStr $ withColor sch namedColor ident
          putStr " = "
          putStrLn $ pretty sch t

implDescribe :: CommandImpl
implDescribe = Impl { implName = "describe"
                    , implFun  = describe
                    , implDoc  = "Display the implementation and type of an identifer"
                    }

preludeSyms :: [Text]
preludeSyms = ["sym", "trans", "cong", "subst", "Assoc", "Comm", "Refl", "Identity", "const", "id", "J"]

agdaPrint :: FilePath -> Text -> TopLevel ()
agdaPrint path ident = do
    ctx <- getCtx
    sch <- getColorScheme

    case OMap.lookup ident (named ctx) of
      Nothing -> errorMsg (T.append ident " is not defined")
      Just (t, ty) -> liftIO $ do
          T.appendFile path $ withColor sch namedColor ident
          T.appendFile path " : "
          T.appendFile path $ pretty sch ty
          T.appendFile path "\n"

          T.appendFile path $ withColor sch namedColor ident
          T.appendFile path " = "
          T.appendFile path $ pretty sch t
          T.appendFile path "\n\n"

agda :: [Expr] -> Opts -> TopLevel ()
agda [(ExprString path)] _ = do
    (Ctx _ fs) <- getCtx
    dir <- liftIO $ getAppUserDataDirectory "proveit"
    prelude <- liftIO . T.readFile $ dir </> "ProveIt.agda"

    let path' = T.unpack path ++ ".agda"

    liftIO $ T.writeFile path' (T.concat ["module ", T.pack (takeFileName (T.unpack path)), " where\n\n"])
    liftIO $ T.appendFile path' prelude

    mapM_ (agdaPrint path') $ (OMap.orderedKeys fs \\ preludeSyms)

agda _ _ = errorMsg "Invalid args to agda command"

implAgda :: CommandImpl
implAgda = Impl { implName = "agda"
                , implFun  = agda
                , implDoc  = "Export to an agda file"
                }

implQuit :: CommandImpl
implQuit = Impl { implName = "quit"
                , implFun  = \_ _ -> liftIO exitSuccess
                , implDoc  = "Exit the ProveIt shell."
                }

defined :: [Expr] -> Opts -> TopLevel ()
defined [(ExprIdent name)] _ = do
    ctx <- getCtx
    sch <- getColorScheme
    putStrLn $
        if OMap.member name (named ctx)
        then withColor sch okColor "YES"
        else withColor sch errorColor "NO"

defined _ opts = do
    state <- getState
    ctx <- getCtx
    let breakChar = if Map.member "long" opts then '\n' else ' '
    putStr . namedColor $ tlColorScheme state
    mapM_ (\name -> putStr name >> putChar breakChar) $ OMap.keys (named ctx)
    putStr sgrReset
    when (breakChar == ' ') $ putChar '\n'

implDefined :: CommandImpl
implDefined = Impl { implName = "defined"
                   , implFun  = defined
                   , implDoc  = T.concat [ "With no arguments, display all defined identifiers.\n"
                                         , "Otherwise test to see if the provided identifier is defined."
                                         ]
                   }

forget :: [Expr] -> Opts -> TopLevel ()
forget [] _= return ()
forget (ExprIdent ident : idents) opts = do
    removeNamedVar ident
    forget idents opts

implForget :: CommandImpl
implForget = Impl { implName = "forget"
                  , implFun  = forget
                  , implDoc  = "Undefine a list of identifiers."
                  }

qed :: [Expr] -> Opts -> TopLevel ()
qed _ opts = do
    metas <- tlMetas <$> getState
    ctx <- getCtx

    when (not (null metas)) $ errorMsg "There are still goals to be proved!"
    theoremInfo <- getCurrentTheorem
    when (isNothing theoremInfo) $ errorMsg "Not currently proving theorem"
    let Just (name, theorem) = theoremInfo

    -- Reconstruct the solved meta variables into a single term.
    solved <- getSolvedMetas
    let Just x = Map.lookup (minimum (Map.keys solved)) solved
        proof  = reconstructMeta solved x

    -- Check that the reconstructed proof is correct.
    verbosity <- getVerbosity
    when (Map.member "auto-sorry" opts) $ setVerbosity Silent
    res <- recover (simpleCheck ctx proof theorem)
    setVerbosity verbosity
    case res of
      Nothing -> if Map.notMember "auto-sorry" opts then end
                 else do
                     runCommand (CmdExpr "sorry" [] Map.empty)
                     runCommand (CmdExpr "qed" [] Map.empty)
      Just _ -> do
          wstats <- equalScriptVar "wstats-on" "YES"
          when wstats $ do
              wtime <- getScriptVar "wstats-wtime"
              rtime <- getScriptVar "wstats-rtime"
              liftIO $ T.appendFile "/tmp/wstats" (T.concat [name, ", ", wtime, ", ", rtime, "\n"])

          addNamedVar name proof theorem
          incScriptVar "theorems"
          isDefinedMsg name
          endCurrentTheorem
          clearSolvedMetas
          clearMetas

implQed :: CommandImpl
implQed = Impl { implName = "qed"
               , implFun  = qed
               , implDoc  = "Finish defining a theorem"
               }

intro :: [Expr] -> Opts -> TopLevel ()
intro idents _ = do
    vs <- T.concat . intersperse " " <$> mapM identToText idents
    runCommand (CmdTerm (T.concat ["\\", vs, " -> ?"]))

implIntro :: CommandImpl
implIntro = Impl { implName = "intro"
                 , implFun  = intro
                 , implDoc  = "intro a b c == \"\\a b c -> ?\""
                 }

normalize :: [Expr] -> Opts -> TopLevel ()
normalize [ExprIdent ident] _ = do
    ctx <- getCtx
    normalForm <- tlnf ctx (Named ident)
    prettyPrint normalForm

implNormalize :: CommandImpl
implNormalize = Impl { implName = "normalize"
                     , implFun  = normalize
                     , implDoc  = "Normalize the term associated with an identifer."
                     }

helpHelp :: Text
helpHelp = "usage: help <command>\nUse 'commands.' to list all commands."

help :: [Expr] -> Opts -> TopLevel ()
help [(ExprIdent ident)] _ = do
    cmd <- lookupCommandImpl ident
    case cmd of
      Just cmd' -> putStrLn $ implDoc cmd'
      Nothing -> errorMsg (T.concat ["command ", ident, " not found!"])

help _ _ = putStrLn helpHelp

implHelp :: CommandImpl
implHelp = Impl { implName = "help"
                , implFun  = help
                , implDoc  = helpHelp
                }

commands :: [Expr] -> Opts -> TopLevel ()
commands _ _ = do
    cmds <- tlCommandImpls <$> getState
    mapM_ (\name -> putStr name >> putChar ' ') (Map.keys cmds)
    putChar '\n'

implCommands :: CommandImpl
implCommands = Impl { implName = "commands"
                    , implFun  = commands
                    , implDoc  = "list all available commands"
                    }


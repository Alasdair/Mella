{-# LANGUAGE OverloadedStrings #-}

module Lang.TopLevel.Command.Waldmeister (waldmeisterCommands) where

import Prelude hiding (putStrLn)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Timeout

import Lang.Util.OMap (OMap)
import qualified Lang.Util.OMap as OMap

import Lang.Term
import Lang.TopLevel.Monad
import Lang.TopLevel.Parser
import Lang.TypeChecker.Monad hiding (parseTerm, putCtx, getCtx, addNamedVar)
import qualified Lang.TypeChecker.TypeChecker as TC
import qualified Lang.Tactic.Waldmeister as Tactic

waldmeisterCommands :: Map Text CommandImpl
waldmeisterCommands = commandsFromList [ implWaldmeister, implWStatsMode, implWDisplayStats ]

implWaldmeister :: CommandImpl
implWaldmeister = Impl { implName = "waldmeister"
                       , implFun  = waldmeister
                       , implDoc  = "Solve an equation with waldmeister."
                       }

firstOpt []    = errorMsg "Empty option"
firstOpt (x:_) = return x

waldmeister :: [Expr] -> Opts -> TopLevel ()
waldmeister _ opts = do
    metas <- getMetas
    case metas of
      [] -> errorMsg "No metas to solve using waldmeister!"
      (MC ctx n ty) : _ -> do
          signature <- mapM (parseTerm ctx <=< identToText) =<< optLookup "signature" opts
          axioms    <- mapM (parseTerm ctx <=< identToText) =<< optLookup "axioms" opts
          timeout   <- fmap (read . T.unpack) $ identToText =<< firstOpt =<< optLookupDefault [(ExprIdent "5")] "timeout" opts
          let kbo = Map.member "kbo" opts

          let tcmState = TC.defaultState { tcmCtx = ctx } :: TCMState IO
          (result, tcmState') <- liftIO $ runTCMT tcmState (Tactic.waldmeister axioms signature timeout kbo ty)

          wstats <- equalScriptVar "wstats-on" "YES"
          verbosity <- getVerbosity
          when wstats $ setVerbosity Silent

          case (result, wstats) of
            (Left err, True)  -> incScriptVar "wstats-tcerror" >> sorry
            (Left err, False) -> formatTypeError err (tcmLog tcmState')
            (Right (Tactic.WMError _), True)  -> incScriptVar "wstats-error" >> sorry
            (Right (Tactic.WMError n), False)  -> errorMsg (T.append "waldmeister failed with code " (T.pack (show n)))
            (Right (Tactic.WMTimeout), True)  -> incScriptVar "wstats-timeouts" >> sorry
            (Right (Tactic.WMTimeout), False) -> errorMsg "waldmeister timed out"
            (Right (Tactic.WMRefuted), True)  -> incScriptVar "wstats-refutes" >> sorry
            (Right (Tactic.WMRefuted), False) -> errorMsg "waldmeister refuted the goal"
            (Right (Tactic.WMTerm solution startTime waldmeisterTime), _) -> do
                let (Ctx _ lemmaCtx) = tcmCtx tcmState'
                    -- Inefficient, now we are using ordered maps.
                    lemmas = Map.toList $ Map.difference (OMap.toMap lemmaCtx) (OMap.toMap (named ctx))

                forM_ lemmas $ \(name, (t, ty)) -> do
                    ctx <- getCtx
                    res <- recover $ simpleCheck ctx t ty
                    case res of
                      Nothing -> if wstats then incScriptVar "wstats-tcerror" >> sorry else end
                      Just _ -> do
                          addNamedVar name t ty
                          isDefinedMsg name

                solveMeta solution
                endTime <- liftIO getCurrentTime
                let reconstructionTime = diffUTCTime endTime startTime
                when wstats $ do
                    modifyScriptVar "wstats-wtime" (T.pack $ show waldmeisterTime)
                    modifyScriptVar "wstats-rtime" (T.pack $ show reconstructionTime)

                addMetas (tcmMetas tcmState')

          setVerbosity verbosity
          newline
  where
    sorry = runCommand (CmdExpr "sorry" [] Map.empty)

wStatsMode :: [Expr] -> Opts -> TopLevel ()
wStatsMode _ _ = do
    addScriptVar "wstats-error" "0"
    addScriptVar "wstats-on" "YES"
    addScriptVar "wstats-timeouts" "0"
    addScriptVar "wstats-refutes" "0"
    addScriptVar "wstats-lemerr" "NO"
    addScriptVar "wstats-tcerror" "0"
    addScriptVar "wstats-wtime" "UNSET"
    addScriptVar "wstats-rtime" "UNSET"

implWStatsMode = Impl { implName = "wstatsmode"
                      , implFun  = wStatsMode
                      , implDoc  = "collect statistics about waldmeister"
                      }

wDisplayStats :: [Expr] -> Opts -> TopLevel ()
wDisplayStats _ _ = do
    wstats <- equalScriptVar "wstats-on" "YES"
    when wstats $ do
        putStrLn . T.append "Timeouts: " =<< getScriptVar "wstats-timeouts"
        putStrLn . T.append "Waldmeister Errors: " =<< getScriptVar "wstats-error"
        putStrLn . T.append "Refutations: " =<< getScriptVar "wstats-refutes"
        putStrLn . T.append "Type Errors: " =<< getScriptVar "wstats-tcerror"
        putStrLn . T.append "Sorries: " =<< getScriptVar "qedsorry"
        putStrLn . T.append "Theorems: " =<< getScriptVar "theorems"

implWDisplayStats = Impl { implName = "wdisplaystats"
                         , implFun  = wDisplayStats
                         , implDoc  = "display waldmeister statistics"
                         }

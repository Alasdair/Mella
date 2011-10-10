{-# LANGUAGE OverloadedStrings, TupleSections, CPP #-}

module Lang.TypeChecker.TypeChecker where

import Control.Applicative
import Control.Arrow (first)
import Data.Functor.Identity (Identity, runIdentity)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Lang.Util.Counter

import Lang.Interactive
import Lang.PrettyPrint
import Lang.Error

import Lang.Term
import Lang.Term.Parser as Parser

import Lang.TypeChecker.Monad
import Lang.TypeChecker.CoC
import Lang.TypeChecker.Equality
import Lang.TypeChecker.Meta

tRules :: (Functor m, Monad m) => [TCRule m]
tRules = metaMetaRule : tRulesNoMeta

tRulesNoMeta :: (Functor m, Monad m) => [TCRule m]
tRulesNoMeta = [tAbsRule, tInfRule, eqReflRule, eqRewriteRule]

iRules :: (Functor m, Monad m) => [IRule m]
iRules = [ tInfIRule, eqIRule, eqJRule ]

defaultState :: (Functor m, Monad m) => TCMState m
defaultState = TCMState 0 emptyCtx tRules iRules [] [] (Counter 0)

instance PrettyPrint LogEntry where
    pretty sch (LTrace n t1 t2 r) = T.concat [ "GOAL ",T.pack (show n) , " = "
                                             , pretty sch t1, " : ", pretty sch t2
                                             , " [", either (\r' -> T.concat ["\n", pretty sch r', "\n"]) (pretty sch) r , "]"
                                             ]
    pretty sch (LText t) = T.append "MSG = " t

#define __ERROR__ (\fun ctx desc -> TypeError desc Map.empty ctx fun "Lang.TypeChecker.TypeChecker" __LINE__)

simpleCheck :: Ctx -> Term -> Term -> Maybe TypeError
simpleCheck ctx t ty = onlyLeft . fst . runIdentity $ runTCMT state (t `hasType` ty)
  where
    onlyLeft (Right _)  = Nothing
    onlyLeft (Left err) = Just err

    state = defaultState { tcmTCRules = tRulesNoMeta, tcmCtx = ctx }

simpleInfer :: Ctx -> Term -> Either TypeError Term
simpleInfer ctx t = fst . runIdentity $ runTCMT state (infer t)
  where
    state = defaultState { tcmTCRules = tRulesNoMeta, tcmCtx = ctx }

{-# LANGUAGE OverloadedStrings, CPP #-}

module Lang.TypeChecker.Meta ( metaMetaRule ) where

import Lang.Term
import Lang.TypeChecker.Monad

metaMeta :: (Functor m, Monad m) => Term -> Term -> TCMT m Bool
metaMeta (Meta n) ty = do
    validType ty
    state <- get
    put $ state { tcmMetas = MC (tcmCtx state) n ty : (tcmMetas state) }
    return True

metaMeta _ _ = return False

metaMetaRule :: (Functor m, Monad m) => TCRule m
metaMetaRule = TCR "Meta-Meta" metaMeta

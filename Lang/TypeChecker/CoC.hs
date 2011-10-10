{-# LANGUAGE OverloadedStrings, CPP #-}

module Lang.TypeChecker.CoC
    ( tInfIRule
    , tInfRule
    , tAbsRule
    ) where

import Control.Applicative
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

import Lang.PrettyPrint
import Lang.Term
import Lang.Term.Parser as Parser
import Lang.TypeChecker.Monad

#define __ERROR__ typeError "Lang.TypeChecker.CoC" __LINE__

-- +----------------------------------------------------------------+
-- + CCω Typing Rules                                               +
-- +----------------------------------------------------------------+

tPi Star Star = Star
tPi (Box n) Star = Star
tPi Star (Box n) = Box n
tPi (Box n) (Box m) = Box (max n m)

inferCoC :: (Functor m, Monad m) => Term -> TCMT m (Maybe Term)
inferCoC (Sort Star) = return . Just $ Sort (Box 0)
inferCoC (Sort (Box n)) = return . Just $ Sort (Box (n + 1))
inferCoC (Unnamed (DB n _)) = Just <$> getUnnamedVar n
inferCoC (Named name) = Just . snd <$> getNamedVar name

inferCoC (Ann t ann) = do
    validType ann
    t `hasType` ann
    return (Just ann)

inferCoC pi@(Pi tag s t) | inf pi = do
    sType <- infer s
    si <- isSort sType
    sj <- withUnnamedVar tag s $ isSort =<< infer t
    return . Just . Sort $ tPi si sj

inferCoC pi@(Pi _ _ _) = __ERROR__ "inferCoC" [("pi", pi)]
                         "{pi}\npi type is not inferable"

inferCoC a@(App f x) | inf f = do
    t <- infer f
    ctx <- getCtx
    case t of
      (Pi _ t1 t2) -> do
          x `hasType` t1
          return . Just $ shift (-1) 0 (subst 0 (shift 1 0 x) t2)
      otherwise -> __ERROR__ "inferCoC" [("a", a)] "Invalid application:\n{a}"

inferCoC a@(App _ _) =
    __ERROR__ "inferCoC" [("a", a)] "application:\n{a}\nis not inferable"

inferCoC _ = return Nothing

tInfIRule :: (Functor m, Monad m) => IRule m
tInfIRule = IR "T-Inf" inferCoC

tApp :: (Functor m, Monad m) => Term -> Term -> TCMT m Bool
tApp (App f x) ty | inf x = do
    validType ty
    xTy <- infer x
    f `hasType` piBinders [("x", xTy)] ty
    return True

tApp _ _ = return False

tAppRule :: (Functor m, Monad m) => TCRule m
tAppRule = TCR "T-App-Extra" tApp

tInf :: (Functor m, Monad m) => Term -> Term -> TCMT m Bool
tInf t ty | inf t = do
    validType ty
    tType <- tnf =<< infer t
    nfTy <- tnf ty
    ctx <- getCtx
    when (tType /= nfTy) $ __ERROR__ "tInf" [("a", tType), ("b", nfTy)]
                           "Inferred type:\n{a}\ndoes not equal\n{b}"
    return True

tInf _ _ = return False

tInfRule :: (Functor m, Monad m) => TCRule m
tInfRule = TCR "T-Inf" tInf

tAbs ::  (Functor m, Monad m) => Term -> Term -> TCMT m Bool
tAbs l@(Lam tag expr) pi@(Pi _ argType exprType) | inf pi = do
    validType argType
    withUnnamedVar tag argType $ expr `hasType` exprType
    return True

tAbs _ _ = return False

tAbsRule :: (Functor m, Monad m) => TCRule m
tAbsRule = TCR "T-Abs" tAbs

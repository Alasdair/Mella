{-# LANGUAGE OverloadedStrings, CPP #-}

module Lang.TypeChecker.Equality
    ( eqReflRule
    , eqIRule
    , eqJRule
    ) where

import Control.Applicative
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as Text

import Lang.Term
import Lang.Term.Parser as Parser
import Lang.TypeChecker.Monad

#define __ERROR__ typeError "Lang.TypeChecker.Equality" __LINE__

inferId (Id ty t1 t2) = do
    s <- validType ty
    t1 `hasType` ty
    t2 `hasType` ty
    return (Just (Sort s))
inferId _ = return Nothing

{-
inferJ J = return (Just "(A : *) (M : A) (C : (y : A) -> Id A M y -> *) -> C M refl -> (N : A) (P : Id A M N) -> C N P")
inferJ _ = return Nothing
-}

inferJ :: (Functor m, Monad m) => Term -> TCMT m (Maybe Term)
inferJ (J a c e x y p) = return $ Just (App (App (App c x) y) p)
inferJ _ = return Nothing

eqJRule :: (Functor m, Monad m) => IRule m
eqJRule = IR "Eq-J" inferJ

eqIRule :: (Functor m, Monad m) => IRule m
eqIRule = IR "Eq-Infer" inferId

eqRefl :: (Functor m, Monad m) => Term -> Term -> TCMT m Bool
eqRefl Refl eq  = do
    eq' <- tnf eq
    case eq' of
      Id ty t1 t2 -> do
          validType ty
          t1 `hasType` ty
          t2 `hasType` ty
          nft1 <- tnf t1
          nft2 <- tnf t2
          when (nft1 /= nft2) $ __ERROR__ "eqRefl" [("t1", t1), ("t2", t2)]
                                "{t1}\ndoes not equal\n{t2}"
          return True
      otherwise -> __ERROR__ "eqRefl" [] "Incorrect type"

eqRefl _ _ = return False

eqReflRule :: (Functor m, Monad m) => TCRule m
eqReflRule = TCR "Eq-Refl" eqRefl

module Lang.TypeChecker.Waldmeister where

import Lang.TypeChecker.TypeChecker

waldmeister :: Text -> Term -> StateT TCMState m Bool
waldmeister = undefined
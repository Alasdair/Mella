{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Lang.TopLevel where

import Prelude hiding (putStrLn, putChar, putStr)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Lang.Util.Counter
import Lang.Util.UndoStateT

import Lang.Interactive
import qualified Lang.PrettyPrint as PP
import Lang.PrettyPrint hiding (colorBanner, prettyPrint)
import Lang.TopLevel.Parser
import Lang.TopLevel.Monad

main :: TopLevel ()
main = do
   preludePath <- tlPrelude <$> getState
   (Just preludeCommands) <- liftIO $ (parseCommands <=< tokenize) <$> T.readFile preludePath
   addScriptVar "theorems" "0"
   mapM_ runCommand preludeCommands
   addScriptVar "qedsorry" "0"
   modifyScriptVar "theorems" "0"
   clearUndoHistory
   loop

loop :: TopLevel ()
loop = do
    prompt <- maybe "λ" (`T.append` " λ") . fmap fst . tlCurrentTheorem <$> getState
    (Counter n) <- getCounter
    let prompt' = "MELLA" -- T.append (T.pack (show n ++ " ")) prompt
    sch <- getColorScheme
    command <- getText sch prompt'
    when (command == "") loop
    checkpoint command
    result <- recover $ case parseCommand =<< tokenize command of
      Just command' -> runCommand command'
      Nothing       -> errorMsg "Failed to parse command!"
    case result of
      Just _  -> return ()
      Nothing -> undo
    metas <- tlMetas <$> getState
    solved <- getSolvedMetas
    theorem <- getCurrentTheorem
    unless (null metas) $ do
        colorBanner okColor "GOALS"
        mapM_  (\(n, meta) -> putStr (T.pack (show n)) >> putStr ": " >> prettyPrint meta) (zip [1..] metas)
    when (null metas && isJust theorem) $ do
        colorBanner okColor "GOALS"
        putStrLn "All solved!"
    loop

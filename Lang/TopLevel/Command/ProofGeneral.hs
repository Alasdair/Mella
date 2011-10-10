{-# LANGUAGE OverloadedStrings #-}

module Lang.TopLevel.Command.ProofGeneral (pgCommands) where

import Prelude hiding (putStrLn, putChar, putStr)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Directory

import Lang.Util.UndoStateT

import Lang.TopLevel.Parser
import Lang.TopLevel.Monad

pgCommands :: Map Text CommandImpl
pgCommands = commandsFromList [ implPGUndo ]

implPGUndo :: CommandImpl
implPGUndo = Impl { implName = "pg-undo"
                  , implFun  = pgUndo
                  , implDoc  = "Proof General internal command"
                  }

pgUndo [ExprString path] _ = do
    text <- liftIO $ T.readFile (T.unpack path)
    case parseCommands =<< tokenize text of
      Just commands -> do
          undo
          replicateM_ (length commands) undo
          putStrLn $ T.concat ["undid ", T.pack (show (length commands)), " commands"]
          liftIO $ removeFile (T.unpack path)
      Nothing -> errorMsg "Invalid undo span!"

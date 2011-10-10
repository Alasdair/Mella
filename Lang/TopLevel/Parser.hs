{-# LANGUAGE OverloadedStrings #-}

module Lang.TopLevel.Parser
    ( parseCommand
    , parseCommands
    , parseFile
    , Tok.tokenize
    , Command (..)
    , Expr (..)
    ) where

import Control.Applicative
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Lang.TopLevel.HParser
import qualified Lang.TopLevel.Tokenizer as Tok

parseFile :: FilePath -> IO (Maybe [Command])
parseFile path = do
    file <- T.readFile path
    return $ do
        toks <- Tok.tokenize file
        parseCommands toks

{-# LANGUAGE OverloadedStrings #-}
module Lang.Interactive where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Attoparsec.Text as Atto
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Console.Haskeline

import Lang.Error
import Lang.Term
import Lang.Term.Parser as Parser
import Lang.TopLevel.Parser as Parser
import Lang.PrettyPrint

getText :: MonadIO m => ColorScheme -> Text -> m Text
getText sch prompt =
    liftIO $ runInputT defaultSettings (getTerm' (T.unpack prompt))
  where
    getTerm' prompt = do
        (Just input) <- getInputLine (concat [ T.unpack (promptColor sch)
                                             , prompt, T.unpack (reset sch)
                                             , "> "
                                             ])
        return (T.pack input)

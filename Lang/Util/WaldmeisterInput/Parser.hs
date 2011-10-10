
module Lang.Util.WaldmeisterInput.Parser
    ( parseFile
    ) where

import Control.Monad

import Lang.Util.WaldmeisterInput.HParser
import Lang.Util.WaldmeisterInput.Tokenizer
import Lang.Error
import Lang.PrettyPrint

import qualified Data.Text.IO as T

parseFile = tokenize >=> parseWMFile

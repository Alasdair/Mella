module Lang.TopLevel.Command
    ( baseCommands
    , waldmeisterCommands
    , eqCommands
    , pgCommands
    , standardCommands
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Lang.TopLevel.Monad
import Lang.TopLevel.Command.Base
import Lang.TopLevel.Command.Waldmeister
import Lang.TopLevel.Command.EqReasoning
import Lang.TopLevel.Command.ProofGeneral

standardCommands :: Map Text CommandImpl
standardCommands = Map.unions [ baseCommands, waldmeisterCommands, eqCommands ]

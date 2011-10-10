{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad

import qualified Data.Map as Map

import Lang.PrettyPrint (noScheme)
import Lang.TopLevel.Command
import Lang.TopLevel.Monad hiding (putStrLn, putStr, putChar)
import qualified Lang.TopLevel as TL (main)

import System.FilePath
import System.Directory
import System.Environment (getArgs)
import System.Console.GetOpt

import Network.HTTP.Wget

data Flag = ProofGeneral deriving (Eq)

flags = [ Option ['p'] ["proof-general"] (NoArg ProofGeneral)
            "Disable coloured output and enable special proof general commands."
        ]

preludeOnline :: String
preludeOnline = "https://raw.github.com/gist/1211182/289c2330ed10c6f57f36901b3eb08e442afa2de8/prelude.prv"

main :: IO ()
main = do
    args <- getArgs
    dir <- getAppUserDataDirectory "proveit"
    let preludePath = dir </> "prelude.prv"

    preludeExist <- doesFileExist preludePath

    when (not preludeExist) $ do
      createDirectoryIfMissing True dir
      putStrLn ("reading prelude from: " ++ preludeOnline)
      prelude <- wget preludeOnline [] []
      writeFile preludePath prelude

    let (setFlags, _, _) = getOpt Permute flags args
        state = if ProofGeneral `elem` setFlags
                then defaultState { tlCommandImpls = standardCommands `Map.union` pgCommands
                                  , tlColorScheme  = noScheme
                                  , tlPrelude = preludePath
                                  }
                else defaultState { tlCommandImpls = standardCommands
                                  , tlPrelude = preludePath
                                  }
    runTopLevel TL.main state
    return ()

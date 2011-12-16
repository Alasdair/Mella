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

flags :: [OptDescr Flag]
flags = [ Option "p" ["proof-general"] (NoArg ProofGeneral)
            "Disable coloured output and enable special proof general commands."
        ]

preludeOnline :: String
preludeOnline = "http://staffwww.dcs.shef.ac.uk/people/A.Armstrong/prelude.mel"

agdaPreludeOnline :: String
agdaPreludeOnline = "http://staffwww.dcs.shef.ac.uk/people/A.Armstrong/Mella.agda"

main :: IO ()
main = do
    args <- getArgs
    dir <- getAppUserDataDirectory "mella"
    let preludePath = dir </> "prelude.mel"
    let agdaPreludePath = dir </> "Mella.agda"

    preludeExist <- doesFileExist preludePath
    agdaPreludeExist <- doesFileExist agdaPreludePath

    unless preludeExist $ do
      createDirectoryIfMissing True dir
      putStrLn ("Reading prelude from: " ++ preludeOnline)
      prelude <- wget preludeOnline [] []
      writeFile preludePath prelude

    unless agdaPreludeExist $ do
      createDirectoryIfMissing True dir
      putStrLn ("Reading agda prelude from: " ++ agdaPreludeOnline)
      prelude <- wget agdaPreludeOnline [] []
      writeFile agdaPreludePath prelude

    let (setFlags, _, _) = getOpt Permute flags args
        state = if ProofGeneral `elem` setFlags
                then defaultState { tlCommandImpls = standardCommands `Map.union` pgCommands
                                  , tlColorScheme  = noScheme
                                  , tlPrelude = preludePath
                                  }
                else defaultState { tlCommandImpls = standardCommands
                                  , tlPrelude = preludePath
                                  }
    _ <- runTopLevel TL.main state
    return ()

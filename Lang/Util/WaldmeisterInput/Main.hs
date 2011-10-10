module Lang.Util.WaldmeisterInput.Main where

import Control.Monad
import Control.Applicative
import Control.Arrow

import Data.List (delete)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Directory
import System.Environment
import System.FilePath
import System.Random

import Lang.PrettyPrint
import Lang.Error
import Lang.Util.WaldmeisterInput.ToProveIt
import Lang.Util.WaldmeisterInput.HParser
import Lang.Util.WaldmeisterInput.Parser

getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive dir = do
    contents <- map (dir </>) . delete "." . delete ".." <$> getDirectoryContents dir
    files    <- filterM doesFileExist contents
    dirs     <- filterM doesDirectoryExist contents
    files'   <- concat <$> mapM getFilesRecursive dirs
    return $ files ++ files'

getFilesRecursiveWithExt :: FilePath -> String -> IO [FilePath]
getFilesRecursiveWithExt dir ext = do
    files <- getFilesRecursive dir
    return $ filter (\file -> takeExtension file == ext) files

names :: StdGen -> ([String], StdGen)
names gen = let (n, gen') = name 10 "proof" gen in first (n :) (names gen')

name :: Int -> String -> StdGen -> (String, StdGen)
name 0 n gen = (n, gen)
name m n gen = let (char, gen') = randomR ('a', 'z') gen
               in name (m - 1) (char : n) gen'

main :: Int -> FilePath -> IO Text
main timeout dir = do
    files    <- getFilesRecursiveWithExt dir ".pr"
    putStrLn $ "Read " ++ show (length files) ++ " waldmeister files"
    contents <- mapM T.readFile files
    parses   <- catMaybes <$> mapM tryParse contents
    putStrLn $ "Sucessfully parsed " ++ show (length parses) ++ " files"
    gen <- getStdGen
    let (ns, gen') = first (map T.pack) $ names gen
    let parses' = map (\(newName, parse) -> parse { wmName = newName }) (zip ns parses)
    return $ T.concat $ map (toProveIt timeout) parses'

-- | Try to parse the contents of a Waldmeister input file. If the contents
--   cannot be parsed, print the error and the contents of the file itself.
tryParse :: Text -> IO (Maybe WMFile)
tryParse wm =
    case runError noScheme $ parseFile wm of
      (Left err) -> do T.putStrLn err
                       putStrLn "=== FILE ==="
                       T.putStrLn wm
                       return Nothing
      (Right parsed) -> return (Just parsed)

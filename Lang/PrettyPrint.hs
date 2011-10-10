{-# LANGUAGE OverloadedStrings #-}

module Lang.PrettyPrint where

import System.Console.ANSI

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data ColorScheme = ColorScheme
    { keywordColor :: Text
    , sortColor :: Text
    , namedColor :: Text
    , unnamedColor :: Text
    , indexColor :: Text
    , promptColor :: Text
    , errorColor :: Text
    , okColor :: Text
    , showIndices :: Bool
    , reset :: Text
    }

banner :: Text -> Text
banner t = T.center 30 '=' $ T.concat [" ", t, " "]

colorBanner :: ColorScheme -> (ColorScheme -> Text) -> Text -> Text
colorBanner sch color t =
    let len = T.length t
        d = (30 - len - 2) `div` 2
        m = (30 - len - 2) `mod` 2
    in T.concat [ T.replicate (d + m) "="
                , color sch, " ", t, " ", reset sch
                , T.replicate d "="
                ]

defaultScheme = ColorScheme
    { keywordColor = T.pack $ setSGRCode [SetColor Foreground Vivid Magenta]
    , sortColor    = T.pack $ setSGRCode [SetColor Foreground Dull Magenta]
    , unnamedColor = T.pack $ setSGRCode [SetColor Foreground Dull Blue]
    , namedColor   = T.pack $ setSGRCode [SetColor Foreground Vivid Yellow]
    , indexColor   = T.pack $ setSGRCode [SetColor Foreground Vivid Red]
    , promptColor  = T.pack $ setSGRCode [SetConsoleIntensity BoldIntensity]
    , errorColor   = T.pack $ setSGRCode [SetColor Foreground Vivid Red]
    , okColor      = T.pack $ setSGRCode [SetColor Foreground Vivid Green]
    , showIndices  = False
    , reset        = sgrReset
    }

debugScheme = defaultScheme { showIndices = True }

noScheme           = ColorScheme
    { keywordColor = ""
    , sortColor    = ""
    , namedColor   = ""
    , unnamedColor = ""
    , indexColor   = ""
    , promptColor  = ""
    , errorColor   = ""
    , okColor      = ""
    , showIndices  = False
    , reset        = ""
    }

sgrReset :: Text
sgrReset = T.pack $ setSGRCode [Reset]

class PrettyPrint a where
    pretty :: ColorScheme -> a -> Text

    prettyBracket :: ColorScheme -> a -> Text
    prettyBracket sch x = T.concat ["(", pretty sch x, ")"]

    prettyPrint :: ColorScheme -> a -> IO ()
    prettyPrint cs = T.putStrLn . pretty cs

withColor :: ColorScheme -> (ColorScheme -> Text) -> Text -> Text
withColor sch color t = T.concat [color sch, t, reset sch]

instance PrettyPrint Text where
    pretty sch t = t

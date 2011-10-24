{-# LANGUAGE OverloadedStrings #-}

module Lang.Error where

import Control.Applicative
import Control.Monad

import Data.Text (Text)

import Lang.PrettyPrint

class ErrorMsg e where
    toPrettyText :: e -> ColorScheme -> Text

newtype Error a = Error (Either (ColorScheme -> Text) a)

instance Functor Error where
    fmap f (Error (Right x)) = Error $ Right (f x)
    fmap f (Error (Left err)) = Error $ Left err

instance Monad Error where
    return x = Error (Right x)

    (Error (Right x)) >>= f = f x
    (Error (Left err)) >>= f = Error $ Left err

instance Applicative Error where
    pure = return

    (<*>) = ap

throwError :: ErrorMsg e => e -> Error a
throwError err = Error . Left $ toPrettyText err

runError :: ColorScheme -> Error a -> Either Text a
runError sch (Error (Right x)) = Right x
runError sch (Error (Left err)) = Left $ err sch

maybeToError :: Maybe a -> Error a
maybeToError (Just x) = Error (Right x)
maybeToError (Nothing) = Error (Left (\_ -> "Nothing"))

fromError :: Error a -> a
fromError (Error (Right x)) = x

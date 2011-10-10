{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Lang.Util.UndoStateT
       ( UndoStateT (..)
       , modifyState
       , getState
       , undo
       , clearUndoHistory
       , showUndoHistory
       , checkpoint
       ) where

import Data.Text (Text)

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State as State

data Change s = Undo (s -> s)
              | Checkpoint Text

newtype UndoStateT s m a = US
    { unUS :: StateT ([Change s], s) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

modifyState :: (Monad m) => (s -> s) -> (s -> s) -> UndoStateT s m ()
modifyState change undo = US $ do
    (undos, state) <- get
    put (Undo undo : undos, change state)

getState :: (Monad m) => UndoStateT s m s
getState = US $ do
    (undos, state) <- get
    return state

undo :: Monad m => UndoStateT s m ()
undo = US $ do
    (undos, state) <- get
    case undos of
      []                  -> return ()
      Checkpoint _ : undos' -> put (undos', state)
      Undo f : undos'     -> put (undos', f state) >> unUS undo

clearUndoHistory :: Monad m => UndoStateT s m ()
clearUndoHistory = US $ do
    (_, state) <- get
    put ([], state)

checkpoint :: Monad m => Text -> UndoStateT s m ()
checkpoint name = US $ do
    (undos, state) <- get
    put (Checkpoint name : undos, state)

showUndoHistory :: Monad m => Int -> UndoStateT s m [Text]
showUndoHistory n = US $ do
    (undos, state) <- get
    return . map (\x -> case x of Checkpoint name -> name; Undo _ -> "undo") $ take n undos

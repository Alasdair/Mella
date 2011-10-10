module Lang.Util.EitherT where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Text (Text)

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

class EitherTErr e where
    textError :: Text -> e

instance (Functor m) => Functor (EitherT e m) where
    fmap f z = EitherT (fmap f <$> runEitherT z)

instance (Applicative m, Monad m) => Applicative (EitherT e m) where
    pure = EitherT . pure . Right

    (<*>) = ap

instance (Monad m) => Monad (EitherT e m) where
    return = EitherT . return . Right

    z >>= f = EitherT . (=<< runEitherT z) $ \c -> case c of
        (Right x) -> runEitherT (f x)
        (Left e) -> return (Left e)

instance MonadTrans (EitherT e) where
    lift m = EitherT $ (return . Right) =<< m

instance MonadIO m => MonadIO (EitherT e m) where
    liftIO m = EitherT $ (return . Right) =<< liftIO m

err :: Monad m => e -> EitherT e m a
err = EitherT . return . Left

{-# LANGUAGE TupleSections #-}

module Synacor.SequentialReader (SequentialReader(..), getPosition, lift, setPosition, sequentially) where

import Control.Applicative (Alternative(..))
import Control.Arrow (second)
import Control.Monad.Fail (MonadFail(..))
import Prelude hiding (fail)

data SequentialReader m k v = SequentialReader {runSequentialReader :: k -> m (k, v)}

sequentially :: (Monad m, Enum k) => (k -> m v) -> SequentialReader m k v
sequentially reader = SequentialReader $ \k -> (succ k,) <$> reader k

getPosition :: (Applicative m) => SequentialReader m k k
getPosition = SequentialReader $ \k -> pure (k, k)

setPosition :: (Applicative m) => k -> SequentialReader m k ()
setPosition = SequentialReader . const . pure . (, ())

lift :: (Functor m) => m v -> SequentialReader m k v
lift m = SequentialReader $ \k -> fmap (k,) m

instance (Functor m) => Functor (SequentialReader m k) where
    fmap f s = SequentialReader $ fmap (second f) . runSequentialReader s

instance (Monad m) => Applicative (SequentialReader m k) where
    pure v = SequentialReader $ \k -> pure (k, v)
    SequentialReader f <*> SequentialReader get = SequentialReader $ \k -> do
        (k', f') <- f k
        (k'', v) <- get k'
        pure (k'', f' v)

instance (Alternative m, Monad m) => Alternative (SequentialReader m k) where
    empty = SequentialReader $ \k -> (k,) <$> empty
    SequentialReader a <|> SequentialReader b = SequentialReader $ \k -> a k <|> b k

instance (Monad m) => Monad (SequentialReader m k) where
    SequentialReader get >>= f = SequentialReader $ \k -> do
        (k', v) <- get k
        runSequentialReader (f v) k'

instance (MonadFail m, Show k) => MonadFail (SequentialReader m k) where
    fail s = SequentialReader $ \k -> fail $ show k ++ ": " ++ s

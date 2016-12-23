module Synacor.SequentialReader (SequentialReader(..), sequentially) where

import Control.Arrow (second)
import Control.Monad.Fail (MonadFail(fail))
import Prelude hiding (fail)

data SequentialReader m k v = SequentialReader {runSequentialReader :: k -> m (k, v)}

sequentially :: (Monad m, Enum k) => (k -> m v) -> SequentialReader m k v
sequentially reader = SequentialReader $ \k -> (,) (succ k) <$> reader k

instance (Functor m) => Functor (SequentialReader m k) where
    fmap f s = SequentialReader $ fmap (second f) . runSequentialReader s

instance (Monad m) => Applicative (SequentialReader m k) where
    pure v = SequentialReader $ \k -> pure (k, v)
    SequentialReader f <*> SequentialReader get = SequentialReader $ \k -> do
        (k', f') <- f k
        (k'', v) <- get k'
        pure (k'', f' v)

instance (Monad m) => Monad (SequentialReader m k) where
    SequentialReader get >>= f = SequentialReader $ \k -> do
        (k', v) <- get k
        let SequentialReader get' = f v
        get' k'

instance (MonadFail m, Show k) => MonadFail (SequentialReader m k) where
    fail s = SequentialReader $ \k -> fail $ show k ++ ": " ++ s

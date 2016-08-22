module Control.Monad.Exception (finallyStateT) where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad.State

-- https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/exceptions-and-monad-transformers
-- show
catchStateT :: Exception e => StateT s IO a -> (e -> StateT s IO a) -> StateT s IO a
catchStateT a onE = do
    s1 <- get
    (result, s2) <- liftIO $ runStateT a s1 `catch` \e ->
        runStateT (onE e) s1
    put s2
    return result

finallyStateT :: StateT s IO a -> StateT s IO b -> StateT s IO a
finallyStateT a sequel = do
    result <- a `catchStateT` \e -> do
        _ignored <- sequel
        liftIO $ throwIO (e :: SomeException)
    _ignored <- sequel
    return result
-- /show

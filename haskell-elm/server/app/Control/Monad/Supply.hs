-- https://wiki.haskell.org/New_monads/MonadSupply
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Supply where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State

-- supply monad transformer
-- s is the type to supply, m is the monad, a is the return value
newtype SupplyT s m a = SupplyT (StateT [s] m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- supply monad itself
-- s is the supply type, a is the return type of the computation
newtype Supply s a = Supply (SupplyT s Identity a)
  deriving (Functor, Applicative, Monad, MonadSupply s)

-- supply is a function that will return a single value of s
class Monad m => MonadSupply s m | m -> s where
  supply :: m s

-- making supplyt an instance of MonadSupply
instance Monad m => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ do
    (x:xs) <- get
    put xs
    return x

evalSupplyT (SupplyT s) supp = evalStateT s supp
evalSupply (Supply s) supp = evalSupplyT s supp

runSupplyT (SupplyT s) supp = runStateT s supp
runSupply (Supply s) supp = runSupplyT s supp

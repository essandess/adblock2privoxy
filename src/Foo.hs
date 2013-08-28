{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Foo where


class (Monad m) => MonadState s m | m -> s where
    -- | Return the state from the internals of the monad.
    get :: m s
    -- | Replace the state inside the monad.
    put :: s -> m ()
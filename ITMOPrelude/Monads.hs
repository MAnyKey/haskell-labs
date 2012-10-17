{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Monads where

import ITMOPrelude.Category

id x = x

class MonadFish m where
  returnFish :: a -> m a
  (>=>) :: (a -> m b) -> (b -> m c) -> a -> m c

instance Monad m => Functor m where
  fmap f x = x >>= f

class Functor m => MonadJoin m where
  returnJoin :: a -> m a
  join :: m (m a) -> m a
  
instance Monad m => MonadFish m where
  returnFish = return
  f >=> g = \a -> f a >>= \b -> g b

instance MonadFish m => Monad m where
  return = returnFish  
  ma >>= f = id >=> f ma

instance MonadJoin m => Monad m where
  return = returnJoin
  f >>= g = join (fmap g f)
{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Monads where

import ITMOPrelude.Categories
import ITMOPrelude.Primitive(($), (.))
id x = x

class MonadFish m where
  returnFish :: a -> m a
  (>=>) :: (a -> m b) -> (b -> m c) -> a -> m c

class Functor m => MonadJoin m where
  returnJoin :: a -> m a
  join :: m (m a) -> m a


-- Proofs

-- (>>=) -> (>=>)
instance Monad m => MonadFish m where
  returnFish = return
  f >=> g = \a -> f a >>= \b -> g b

-- (>>=) -> join
instance Monad m => Functor m where
  fmap f x = x >>= (return . f)

instance Monad m => MonadJoin m where
  returnJoin = return
  join ma = ma >>= id

{-
-- (>=>) -> (>>=)
instance MonadFish m => Monad m where
  return = returnFish  
  ma >>= f = id >=> f ma
-}

-- join -> (>>=)
instance MonadJoin m => Monad m where
  return = returnJoin
  f >>= g = join (fmap g f)
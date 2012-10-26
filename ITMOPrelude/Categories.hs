{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree

-- class Category cat where
--   id :: cat a a
--   (.) :: cat b c -> cat a b -> cat a c


class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  
(>>) :: (Monad m) => m a -> m b -> m b
a >> b = a >>= (\_ -> b)

-- instances

instance Functor List where
  fmap = map

-- instance Applicative List where
--   pure = flip Cons Nil
--   fs <*> xs = concatMap (flip map xs) fs
  
instance Monad List where
  return x = Cons x Nil
  as >>= f = concatMap f as
  
instance Functor Maybe where
  fmap f (Just a) = Just $ f a
  fmap f Nothing = Nothing

-- instance Applicative Maybe where
--   pure = Just
--   Nothing <*> _ = Nothing
--   (Just f) <*> x = fmap f x

instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  (Just x) >>= f = f x

instance Functor Tree where
  fmap = tmap


--------------------------------------------------------------------------------
-- Монада State

newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
    return a = State $ \s -> (s, a)
    ma >>= f = State $ \s -> let (s', a) = runState ma s 
                             in runState (f a) s'

                              
{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories.MonadFish

import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadJoin

instance Monad m => MonadFish m where
  returnFish = return
  f >=> g = \a -> f a >>= g

{-
instance MonadJoin m => MonadFish m where
  returnFish = returnJoin
  f >=> g = \a -> join (fmap g (f a))
-}

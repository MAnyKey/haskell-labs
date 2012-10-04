{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Algebra where

class Monoid m where
      mempty :: m
      mappend :: m -> m -> m


class Monoid g => Group g where
      gempty :: g
      ginv :: g -> g
      gappend :: g -> g -> g
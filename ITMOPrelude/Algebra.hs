{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Algebra where

import ITMOPrelude.Primitive

import ITMOPrelude.List
import ITMOPrelude.Tree

class Monoid m where
      mempty :: m
      mappend :: m -> m -> m

class Monoid g => Group g where
      ginv :: g -> g

instance Monoid Nat where 
  mempty = Zero
  mappend = (+.)

instance Monoid Int where
  mempty = intZero
  mappend = (.+.)

instance Group Int where
  ginv = intNeg

data MulInt = Mul Int

instance Monoid MulInt where
  mempty = Mul intOne
  mappend (Mul a) (Mul b) = Mul $ a .*. b

instance Monoid Rat where
  mempty = ratZero
  mappend = (%+)

instance Group Rat where
  ginv = ratNeg

data MulRat = MulRat Rat

instance Monoid MulRat where
  mempty = MulRat ratOne
  mappend (MulRat a) (MulRat b) = MulRat $ a %* b

instance Group MulRat where
  ginv (MulRat a) = MulRat $ ratInv a
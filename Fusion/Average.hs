module Fusion.Average where

import Fusion.Common


data StrictTr a b c = StTr !a !b !c

movingAverage :: (Fractional a) => Stream a -> Stream a
movingAverage (Stream nextf0 s0) = Stream nextf (StTr 0 0 s0) where
  nextf (StTr i pr s) = case nextf0 s of
    Done -> Done
    Skip s' -> Skip (StTr i pr s')
    Yield v s' -> Yield newvalue (StTr (i + 1) newvalue s')
      where newvalue = (v + i * pr) / (i + 1)
{-# INLINE movingAverage #-}
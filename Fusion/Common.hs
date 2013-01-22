{-# LANGUAGE BangPatterns, ExistentialQuantification #-}
module Fusion.Common where



data Step s a = Done
              | Skip !s
              | Yield !a !s


data Stream a = forall s . Stream (s -> Step s a) !s

eq :: (Eq a) => Stream a -> Stream a -> Bool
eq (Stream func1 st1) (Stream func2 st2) = loop (func1 st1) (func2 st2) where 
  loop Done Done = True
  loop Done _    = False
  loop _    Done = False
  loop (Skip ns1) (Skip ns2) = loop (func1 ns1) (func2 ns2)
  loop (Skip ns1) x = loop (func1 ns1) x
  loop x (Skip ns2) = loop x (func2 ns2)
  loop (Yield a1 ns1) (Yield a2 ns2) = a1 == a1 && loop (func1 ns1) (func2 ns2)

cmp :: (Ord a) => Stream a -> Stream a -> Ordering
cmp (Stream func1 st1) (Stream func2 st2) = loop (func1 st1) (func2 st2) where
  loop Done Done = EQ
  loop Done _    = LT
  loop _    Done = GT
  loop (Skip ns1) (Skip ns2) = loop (func1 ns1) (func2 ns2)
  loop (Skip ns1) x = loop (func1 ns1) x
  loop x (Skip ns2) = loop x (func2 ns2)
  loop (Yield a1 ns1) (Yield a2 ns2) = case compare a1 a2 of
    EQ -> loop (func1 ns1) (func2 ns2)
    other -> other


instance (Eq a) => Eq (Stream a) where
  (==) = eq
instance (Ord a) => Ord (Stream a) where
  compare = cmp

empty :: Stream a
empty = Stream nextf ()
  where nextf _ = Done

singleton :: a -> Stream a
singleton x = Stream nextf False
  where nextf False = Yield x True
        nextf True = Done

streamList :: [a] -> Stream a
streamList s = Stream nextf s
  where nextf [] = Done
        nextf (x:xs) = Yield x xs

unstreamList :: Stream a -> [a]
unstreamList (Stream next s) = unfold s
  where unfold !s = case next s of
          Done -> []
          Skip s' -> unfold s'
          Yield x s' -> x : unfold s'

{-# RULES "STREAM streamList/unstreamList fusion" forall s. streamList (unstreamList s) = s #-}
          

data C s = C0 !s
         | C1 !s

cons :: a -> Stream a -> Stream a
cons !w (Stream nextf0 st) = Stream nextf (C1 st) 
  where nextf (C1 s) = Yield w (C0 s)
        nextf (C0 s) = case nextf0 s of
          Done -> Done
          Skip s' -> Skip (C0 s')
          Yield x s' -> Yield x (C0 s')

streamLast :: Stream a -> a
streamLast (Stream next s0) = loop0_last s0
    where
      loop0_last !s = case next s of
                        Done       -> emptyError "last"
                        Skip s'    -> loop0_last  s'
                        Yield x s' -> loop_last x s'
      loop_last !x !s = case next s of
                         Done        -> x
                         Skip s'     -> loop_last x  s'
                         Yield x' s' -> loop_last x' s'
{-# INLINE streamLast #-}

streamEnumFromTo :: (Enum a) => a -> a -> Stream a
streamEnumFromTo from to = Stream nextf (fromEnum from)
  where !enumTo = fromEnum to
        nextf i = if i > enumTo
                  then Done
                  else Yield (toEnum i) (i + 1)

streamForM :: Monad m => (a -> m ()) -> Stream a -> m ()
streamForM func (Stream nextf s0) = loop s0 where
  loop !s = case nextf s of
    Done -> return ()
    Skip s' -> loop s'
    Yield v s' -> do { func v; loop s'; }
    
streamError :: String -> String -> a
streamError func msg = error $ "Fusion.Common." ++ func ++ ": " ++ msg

emptyError :: String -> a
emptyError func = internalError func "Empty input"

internalError :: String -> a
internalError func = streamError func "Internal error"

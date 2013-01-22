{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive hiding ((.))
import ITMOPrelude.List
import ITMOPrelude.Categories

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat }

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State getNat' 
  where getNat' world = (newWorld, value)
          where newWorld = RealWorld (tail . stdIn $ world) (stdOut world) (exitCode world)
                value = head . stdIn $ world

putNat :: Nat -> IO ()
putNat n = State putNat'
  where putNat' world = (newWorld, ())
          where newWorld = RealWorld (stdIn world) (n `Cons` stdOut world) (exitCode world)

setExitCode :: Nat -> IO ()
setExitCode n = State setExitCode'
  where setExitCode' world = (newWorld, ())
          where newWorld = RealWorld (stdIn world) (stdOut world) n

testInfinite :: List Nat -> IO ()
testInfinite Nil = return ()
testInfinite (Cons x xs) = putNat x >> testInfinite xs
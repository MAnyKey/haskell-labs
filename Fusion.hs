{-# LANGUAGE MagicHash #-}
module Fusion 
(
  Stream(..),
  Step(..),
  empty,
  singleton,
  streamList,
  unstreamList,
  
  cons,
  streamLast,
  
  streamEnumFromTo,
  streamForM,
  
  
  movingAverage
) where 

import Fusion.Common
import Fusion.Average
import GHC.Prim
import qualified Prelude as P

stream :: ByteArray# -> Stream P.Int
stream = P.undefined
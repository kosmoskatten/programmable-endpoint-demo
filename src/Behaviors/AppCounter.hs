module Behaviors.AppCounter
       ( AppCounter (..)
       ) where

import Simulation.Node.Counter (Counter (..))
import GHC.Int

-- | Application counter.
data AppCounter =
  AppCounter { receivedBytes :: !Int64 }
  deriving Show
           
-- | Counter instance for the application counter
instance Counter AppCounter where
  empty                = AppCounter 0
  addReceived amount c = c { receivedBytes = receivedBytes c + fromIntegral amount }
  getReceived          = receivedBytes
module Behaviors.Counter
       ( Counter (..)
       ) where

import Data.Time (DiffTime, secondsToDiffTime)
import Simulation.Node.Endpoint.AppCounter (AppCounter (create))
import GHC.Int

-- | Application counter.
data Counter = 
  Counter { numPageFetches   :: !Int64
          , totalFetchTime   :: !DiffTime
          , longestFetchTime :: !DiffTime }
  deriving (Eq, Show)
           
-- | AppCounter instance for the application counter
instance AppCounter Counter where
   create = Counter 0 (secondsToDiffTime 0) (secondsToDiffTime 0)

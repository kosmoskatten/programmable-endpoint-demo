module Behaviors.Counter
       ( Counter (..)
       ) where

import Simulation.Node.Endpoint.AppCounter (AppCounter (create))
import GHC.Int

-- | Application counter.
data Counter = Counter
  deriving (Eq, Show)
           
-- | AppCounter instance for the application counter
instance AppCounter Counter where
   create = Counter 

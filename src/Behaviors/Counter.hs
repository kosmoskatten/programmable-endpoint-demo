module Behaviors.Counter
       ( Counter (..)
       , incNumPageFetches
       , addTotalFetchTime
       , maybeAdjustLongestFetchTime
       ) where

import Data.Time (NominalDiffTime, diffUTCTime)
import Simulation.Node.Endpoint.AppCounter (AppCounter (create))

-- | Application counter.
data Counter = 
  Counter { numPageFetches   :: !Int
          , totalFetchTime   :: !NominalDiffTime
          , longestFetchTime :: !NominalDiffTime }
  deriving (Eq, Show)
           
-- | AppCounter instance for the application counter
instance AppCounter Counter where
   create = Counter 0 zeroTime zeroTime
   
-- | Increase the number of page fetches.
incNumPageFetches :: Counter -> Counter
incNumPageFetches c = c { numPageFetches = numPageFetches c + 1 }

-- | Add to the total fetch time.
addTotalFetchTime :: NominalDiffTime -> Counter -> Counter
addTotalFetchTime duration c = 
  c { totalFetchTime = totalFetchTime c + duration }

-- | Adjust the longest fetch time iff the supplied duration is
-- greater than the old one.
maybeAdjustLongestFetchTime :: NominalDiffTime -> Counter -> Counter
maybeAdjustLongestFetchTime duration c
  | duration > longestFetchTime c =
    c { longestFetchTime = duration }
  | otherwise                     = c

-- | Make a zero valued NominalDiffTime. This is a workaround as the
-- type is lacking a Read instance.
zeroTime :: NominalDiffTime
zeroTime = let t = read "0000-00-00 00:00:00.0 UTC"
           in t `diffUTCTime` t
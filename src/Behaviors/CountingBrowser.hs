module Behaviors.CountingBrowser where

import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Data.Text (Text)
import Data.Time (getCurrentTime, diffUTCTime)
import Simulation.Node.Endpoint.AppCounter
import Simulation.Node.Endpoint.Behavior
import Simulation.Node.Endpoint.Behavior.Browser (browsePage)
import Behaviors.Counter

browsePageCounted :: (BehaviorState s) => Text -> Behavior Counter s [Text]
browsePageCounted url = do
  start <- liftIO getCurrentTime
  pages <- browsePage url
  stop <- liftIO getCurrentTime
  let duration = stop `diffUTCTime` start
  modifyCounter $ maybeAdjustLongestFetchTime duration .
                  addTotalFetchTime duration .
                  incNumPageFetches
  return pages
  
modifyCounter :: (AppCounter c, BehaviorState s) => (c -> c) -> Behavior c s ()
modifyCounter g = do
  counter <- appCounter
  liftIO $ modifyTVarIO' counter g

modifyTVarIO' :: TVar a -> (a -> a) -> IO ()
modifyTVarIO' tvar g = atomically $ modifyTVar' tvar g

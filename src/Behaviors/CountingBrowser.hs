module Behaviors.CountingBrowser where

import Data.Text (Text)
import Simulation.Node.Endpoint.Behavior
import Simulation.Node.Endpoint.Behavior.Browser (browsePage)
import Behaviors.Counter

browsePageCounted :: (BehaviorState s) => Text -> Behavior Counter s [Text]
browsePageCounted = browsePage
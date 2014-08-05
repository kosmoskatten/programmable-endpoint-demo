{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Behaviors.SlowSurfer where

import Behaviors.AppCounter
import Control.Applicative
import Control.Monad
import Data.Text
import Simulation.Node.Endpoint.Behavior 
import Simulation.Node.Endpoint.Behavior.Browser

data SlowSurferState =
  SlowSurferState
  { nextPage      :: !Text
  , shortInterval :: !Int
  , longInterval  :: !Int }
  deriving Show
           
instance BehaviorState SlowSurferState where
  fetch = do
    state <- SlowSurferState <$> oneOfIO ["/texttv/1.html", "/blogger/1.html"] 
                             <*> interval (15, 30)
                             <*> interval (40, 90)
    return ("Slow surfer", state)

slowSurfer :: Behavior AppCounter SlowSurferState ()
slowSurfer =
  forever $ do
    state <- get
    page  <- oneOf =<< browsePage (nextPage state)
    sleepSec $ shortInterval state
    page' <- oneOf =<< browsePage page
    put $ state {nextPage = page'}
    sleepSec $ longInterval state           

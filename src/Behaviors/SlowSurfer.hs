{-# LANGUAGE OverloadedStrings #-}
module Behaviors.SlowSurfer 
       ( SlowSurferState
       , slowSurfer
       ) where

import Control.Applicative
import Control.Monad
import Data.Text
import Simulation.Node.Counter
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
                             <*> interval (30, 90)
                             <*> interval (180, 3600)
    return ("Slow surfer", state)

slowSurfer :: Counter c => Behavior c SlowSurferState ()
slowSurfer =
  forever $ do
    state <- get
    page  <- oneOf =<< browsePage (nextPage state)
    sleepSec $ shortInterval state
    page' <- oneOf =<< browsePage page
    put $ state {nextPage = page'}
    sleepSec $ longInterval state           

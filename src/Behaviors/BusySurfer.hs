{-# LANGUAGE OverloadedStrings #-}
module Behaviors.BusySurfer
       ( BusySurferState
       , busySurfer
       ) where

import Control.Applicative
import Control.Monad
import Data.Text
import Simulation.Node.Counter
import Simulation.Node.Endpoint.Behavior
import Simulation.Node.Endpoint.Behavior.Browser

data BusySurferState =
  BusySurferState
  { nextPage     :: !Text
  , waitInterval :: !Int }
  deriving Show
           
instance BehaviorState BusySurferState where           
  fetch = do
    state <- BusySurferState <$> oneOfIO [ "/texttv/1.html"
                                         , "/blogger/1.html"
                                         , "/blogger/1.html"
                                         , "/blogger/1.html" ]
                             <*> interval (15, 45)
    return ("Busy surfer", state)
           
busySurfer :: Counter c => Behavior c BusySurferState ()
busySurfer = 
  forever $ do
    state <- get
    page <- oneOf =<< browsePage (nextPage state)
    put $ state {nextPage = page}
    sleepSec $ waitInterval state
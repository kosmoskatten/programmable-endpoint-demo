{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Behaviors.SlowSurfer where

import Behaviors.AppCounter
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forever)
import Data.Text (Text)
import Simulation.Node.Counter (Counter ())
import Simulation.Node.Endpoint.Behavior 
  ( Behavior
  , BehaviorState (..)
  , get
  , put
  , sleepSec
  , liftIO
  )
import Simulation.Node.Endpoint.Behavior.Browser (browsePage)
import System.Random

data SlowSurferState =
  SlowSurferState
  { nextPage      :: !Text
  , shortInterval :: !Int
  , longInterval  :: !Int }
  deriving Show
           
instance BehaviorState SlowSurferState where
  fetch = do
    state <- SlowSurferState <$> oneOfIO ["/texttv/1.html"] 
                             <*> interval (10, 20)
                             <*> interval (40, 80)
    return ("Slow surfer", state)

slowSurfer :: Behavior AppCounter SlowSurferState ()
slowSurfer = do
  get >>= liftIO . print
  forever $ do
    state <- get
    page <- oneOf =<< browsePage (nextPage state)
    sleepSec $ shortInterval state
    page' <- oneOf =<< browsePage page
    put $ state {nextPage = page'}
    sleepSec $ longInterval state
           
interval :: (Int, Int) -> IO Int
interval = randomRIO

oneOfIO :: [a] -> IO a
oneOfIO xs = (xs !!) `fmap` randomRIO (0, length xs - 1)

oneOf :: (BehaviorState s, Counter c) => [a] -> Behavior c s a
oneOf = liftIO . oneOfIO
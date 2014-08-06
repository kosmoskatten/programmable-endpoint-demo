{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Concurrent.Async ( Concurrently (..)
                                , async
                                , wait
                                , runConcurrently )
import Control.Monad (void, replicateM_)
import Simulation.Node 
  ( Node
  , counter
  , create
  , activateHttpServices
  , as
  , createEndpoint )
import Simulation.Node.Counter (Counter (..))
import Simulation.Node.Endpoint (Endpoint, addBehavior)
import Simulation.Node.Endpoint.Behavior (Behavior, BehaviorState)
import Behaviors.AppCounter (AppCounter (..))
import Behaviors.BusySurfer
import Behaviors.SlowSurfer
import qualified Services.TextOnly as TextOnly
import qualified Services.TextAndImages as TextAndImages
import System.IO (stderr, hPutStrLn)
import System.Random (randomRIO)
import Text.Printf
import GHC.Int

main :: IO ()
main = do
  -- Create the local node, using the specified gateway address for
  -- use of services.
  node <- create "127.0.0.1" 8888 :: IO (Node AppCounter)
  
  -- Activate node statistics displaying.
  task <- async $ nodeStatistics node
  
  -- Activate the services at the local node.
  activateHttpServices node 8888 [ TextOnly.routes `as` "texttv"
                                 , TextAndImages.routes `as` "blogger" ]
  threadDelay 1000000

  -- Create endpoint in the node.
  ep <- createEndpoint "127.0.0.1" node
  
  -- Add behaviors.
  --void $ slowlyAdd 4 slowSurfer ep
  void $ 
    runConcurrently $ (,) <$> Concurrently (slowlyAdd 1000 slowSurfer ep)
                          <*> Concurrently (slowlyAdd 1000 busySurfer ep)
    
  putStrLn "-------------------->"
  
  -- Wait until ^C.
  wait task

nodeStatistics :: Counter c => Node c -> IO ()
nodeStatistics node =
  nodeStatistics' (counter node) 0
  where
    nodeStatistics' :: Counter c => TVar c -> Int64 -> IO ()
    nodeStatistics' tvar amount = do
      amount' <- getReceived <$> readTVarIO tvar
      let delta = amount' - amount
      hPutStrLn stderr $ printf "Total amount: %d, last second: %d" amount' delta
      threadDelay 1000000
      nodeStatistics' tvar amount'
  
slowlyAdd :: (Counter c, BehaviorState s) =>
             Int -> Behavior c s () -> Endpoint c -> IO ()
slowlyAdd num behavior endpoint =
  replicateM_ num $ do
    delay <- randomRIO (1000, 60000)
    void $ addBehavior behavior endpoint
    threadDelay delay

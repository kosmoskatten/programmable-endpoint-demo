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
  ( Node (counter, endpoints)
  , create
  , activateHttpServices
  , as
  , createEndpoint )
import Simulation.Node.SystemCounter (SystemCounter (..))
import Simulation.Node.Endpoint (Endpoint (behaviors), addBehavior)
import Simulation.Node.Endpoint.AppCounter (AppCounter)
import Simulation.Node.Endpoint.Behavior (Behavior, BehaviorState)
import Simulation.Node.Endpoint.Behavior.Descriptor (Descriptor (appCounter))
import Behaviors.Counter (Counter)
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
  node <- create "127.0.0.1" 8888 :: IO (Node Counter)
  
  -- Activate node statistics displaying.
  task <- async $ watchStatistics node
  
  -- Activate the services at the local node.
  activateHttpServices node 8888 [ TextOnly.routes `as` "texttv"
                                 , TextAndImages.routes `as` "blogger" ]
  threadDelay 1000000

  -- Create endpoint in the node.
  ep <- createEndpoint "127.0.0.1" node
  ep2 <- createEndpoint "127.0.0.1" node
  
  -- Add behaviors.
  void $ slowlyAdd 4 slowSurfer ep
  void $ slowlyAdd 4 busySurfer ep2
--  void $ 
--    runConcurrently $ (,) <$> Concurrently (slowlyAdd 2000 busySurfer ep)
--                          <*> Concurrently (slowlyAdd 2000 busySurfer ep2)
    
  putStrLn "-------------------->"
  
  -- Wait until ^C.
  wait task

watchStatistics :: Node Counter -> IO ()
watchStatistics = go 0
  where
    go :: Int64 -> Node Counter -> IO ()
    go amount node' = do
      amount' <- displayDownlink node' amount
      endpoints' <- readTVarIO (endpoints node')
      let descriptors =  map behaviors endpoints' -- [TVar [Desc]]
      descriptors' <- concat <$> mapM readTVarIO descriptors -- [Desc]
      mapM_ displayCounter descriptors'
      threadDelay 1000000
      go amount' node'

displayDownlink :: Node Counter -> Int64 -> IO Int64
displayDownlink node amount = do
  amount' <- bytesReceived <$> readTVarIO (counter node)
  let delta = amount' - amount
  hPutStrLn stderr $
    printf "Total amount: %d, last second: %d" amount' delta
  return amount'

displayCounter :: Descriptor Counter -> IO ()
displayCounter descriptor = do
  c <- readTVarIO (appCounter descriptor)
  hPutStrLn stderr $ show c

slowlyAdd :: (AppCounter c, BehaviorState s) =>
             Int -> Behavior c s () -> Endpoint c -> IO ()
slowlyAdd num behavior endpoint =
  replicateM_ num $ do
    delay <- randomRIO (1000, 60000)
    void $ addBehavior behavior endpoint
    threadDelay delay

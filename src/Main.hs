{-# LANGUAGE OverloadedStrings #-}
module Main where

import Simulation.Node (Node, create, activateHttpServices, as)

import Behaviors.AppCounter (AppCounter (..))
import qualified Services.TextOnly as TextOnly

main :: IO ()
main = do
  -- Create the local node, using the specified gateway address for
  -- use of services.
  node <- create "127.0.0.1" 8888 :: IO (Node AppCounter)
  
  -- Activate the services at the local node.
  activateHttpServices node 8888 [ TextOnly.routes `as` "texttv" ]
  readLn


module Main
  where

import System.Environment (getArgs)

import Control.Concurrent.MVar

import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node hiding (newLocalNode)

import Data.Text.Lazy (pack, unpack)

import Hive.Queen
import Hive.Drone
import Hive.Client
import Hive.Types

import Hive.RemoteTable (remoteTable)

-------------------------------------------------------------------------------

main :: IO ()
main = do
  let rt = remoteTable initRemoteTable
  args <- getArgs
  case args of
    ["queen", host, port] -> do
      context <- initializeBackend host port rt
      node    <- newLocalNode context

      runProcess node $ startQueen context

    ["drone", host, port] -> do
      context <- initializeBackend host port rt
      node    <- newLocalNode context

      runProcess node $ startDrone context
    
    ["client", host, port, problem] -> do
      context <- initializeBackend host port rt
      node    <- newLocalNode context
      mvar    <- newEmptyMVar

      runProcess node $ solveRequest context (Problem TSP (Instance $ pack problem)) mvar 5000000
      putStrLn . unpack . unSolution =<< takeMVar mvar

    other ->
      putStrLn $ "Your arguments are invalid: " ++ show other
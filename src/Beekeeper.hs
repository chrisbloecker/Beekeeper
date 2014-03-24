module Main
  where

import System.Environment (getArgs)

import Control.Concurrent.MVar
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node hiding (newLocalNode)

import Data.Time      (getCurrentTime, diffUTCTime)
import Data.Text.Lazy (pack, unpack)

import Hive.Queen
import Hive.Drone
import Hive.Client
import Hive.Types

import Hive.RemoteTable (remoteTable)

import qualified Data.Text.Lazy.IO as IOText (readFile)

-------------------------------------------------------------------------------

main :: IO ()
main = do
  let rt = remoteTable initRemoteTable
  args <- getArgs
  case args of
    ("help":_) -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "'queen' host port"
      putStrLn "  to start a queen on host using port"
      putStrLn ""
      putStrLn "'drone' host port"
      putStrLn "  to start a drone on host using port"
      putStrLn ""
      putStrLn "'client host port problem"
      putStrLn "  to start a client on host using port, requestion to solve Problem"

    ["queen", host, port] -> do
      context <- initializeBackend host port rt
      node    <- newLocalNode context

      runProcess node $ runQueen context

    ["drone", host, port] -> do
      context <- initializeBackend host port rt
      node    <- newLocalNode context

      runProcess node $ runDrone context (milliseconds 500)
    
    ["client", host, port, problem] -> do
      context <- initializeBackend host port rt
      node    <- newLocalNode context
      mvar    <- newEmptyMVar

      runProcess node $ solveRequest context (Problem TSP (Instance $ pack problem)) mvar (milliseconds 500) (minutes 1)
      putStrLn . unpack . unSolution =<< takeMVar mvar

    ["cli", host, port, filepath] -> do
      context <- initializeBackend host port rt
      node    <- newLocalNode context
      mvar    <- newEmptyMVar

      fileContent <- IOText.readFile filepath
      start <- getCurrentTime
      runProcess node $ solveRequest context (Problem SSSP (Instance fileContent)) mvar (milliseconds 500) (minutes 2)
      solution <- takeMVar mvar
      end <- getCurrentTime
      case solution of
        Solution _ _   -> return ()
        NoSolution     -> putStrLn "Sorry, we couldn't find a solution."
        InvalidInput   -> putStrLn "Your input is invalid."
        TimeoutReached -> putStrLn "Timeout Reached."
        NotImplemented -> putStrLn "This is not yet implemented."

      putStrLn $ "This took " ++ show (diffUTCTime end start)
      
    other ->
      putStrLn $ "Your arguments are invalid: " ++ show other

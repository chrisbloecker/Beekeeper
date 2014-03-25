module Main
  where

import System.Environment (getArgs)

import Control.Concurrent.MVar
--import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node
import Network.Transport.TCP

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
      putStrLn "'drone' host port queenAddress"
      putStrLn "  to start a drone on host using port"
      putStrLn ""
      putStrLn "'client host port problem"
      putStrLn "  to start a client on host using port, requestion to solve Problem"

    ["queen", host, port] -> do
      Right transport <- createTransport host port defaultTCPParameters
      node    <- newLocalNode transport rt

      runProcess node runQueen

    ["drone", host, port, queenAddress] -> do
      Right transport <- createTransport host port defaultTCPParameters
      node    <- newLocalNode transport rt

      runProcess node $ runDrone queenAddress
    
    ["client", host, port, problem, queenAddress] -> do
      Right transport <- createTransport host port defaultTCPParameters
      node    <- newLocalNode transport rt
      mvar    <- newEmptyMVar

      runProcess node $ solveRequest queenAddress (Problem TSP (Instance $ pack problem)) mvar (minutes 1)
      putStrLn . unpack . unSolution =<< takeMVar mvar

    ["cli", host, port, queenAddress, filepath] -> do
      Right transport <- createTransport host port defaultTCPParameters
      node    <- newLocalNode transport rt
      mvar    <- newEmptyMVar

      fileContent <- IOText.readFile filepath
      start <- getCurrentTime
      runProcess node $ solveRequest queenAddress (Problem SSSP (Instance fileContent)) mvar (minutes 2)
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
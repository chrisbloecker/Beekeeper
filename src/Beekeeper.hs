module Main
  where

import System.Environment (getArgs)

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Distributed.Process.Node
import Network.Transport.TCP

import Data.Time      (getCurrentTime, diffUTCTime)
import Data.Text.Lazy (pack, unpack)

import Hive.Queen  (runQueen)
import Hive.Drone  (runDrone)
import Hive.Client (solveRequest)
import Hive.Types

import Hive.RemoteTable (remoteTable)

import qualified Data.Text.Lazy.IO as IOText (readFile)

-------------------------------------------------------------------------------

mkNode :: String -> String -> IO LocalNode
mkNode host port = do
  Right transport <- createTransport host port defaultTCPParameters
  newLocalNode transport (remoteTable initRemoteTable) -- ToDo: will this cause problems?

help :: IO ()
help = do
  putStrLn "Usage:"
  putStrLn ""
  putStrLn "'queen' host port"
  putStrLn "  to start a queen on host using port"
  putStrLn ""
  putStrLn "'drone' host port queenHost queenPort"
  putStrLn "  to start a drone on host using port"
  putStrLn ""
  putStrLn "'client host port problem"
  putStrLn "  to start a client on host using port, requestion to solve Problem"

handleSolution :: Solution -> IO ()
handleSolution (Solution _ _)  = return ()
handleSolution  NoSolution     = putStrLn "Sorry, we couldn't find a solution."
handleSolution  InvalidInput   = putStrLn "Your input is invalid."
handleSolution  TimeoutReached = putStrLn "Timeout Reached."
handleSolution  NotImplemented = putStrLn "This is not yet implemented."

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("help":_) -> help

    ["queen", host, port] -> do
      node <- mkNode host port
      runProcess node runQueen

    ["drone", host, port, queenHost, queenPort] -> do
      node <- mkNode host port
      runProcess node $ runDrone queenHost queenPort
    
    ["client", host, port, problem, queenHost, queenPort] -> do
      node <- mkNode host port
      mvar <- newEmptyMVar
      runProcess node $ solveRequest queenHost queenPort (Problem TSP (Instance $ pack problem)) mvar (minutes 1)
      putStrLn . unpack . unSolution =<< takeMVar mvar

    ["cli", host, port, queenHost, queenPort, filepath] -> do
      node <- mkNode host port
      mvar <- newEmptyMVar

      fileContent <- IOText.readFile filepath
      start       <- getCurrentTime
      runProcess node $ solveRequest queenHost queenPort (Problem SSSP (Instance fileContent)) mvar (minutes 2)
      handleSolution =<< takeMVar mvar
      end         <- getCurrentTime

      putStrLn $ "This took " ++ show (diffUTCTime end start)
      
    other ->
      putStrLn $ "Your arguments are invalid: " ++ show other
module Main
  where

import System.Environment (getArgs)

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Distributed.Process.Node
import Network.Transport.TCP

import Data.Time      (getCurrentTime, diffUTCTime)
import Data.Text.Lazy (pack)

import Hive.Master (runMaster)
import Hive.Node   (runNode)
import Hive.Client (solveRequest)
import Hive.Types

import Hive.RemoteTable (remoteTable)

import qualified Data.Text.Lazy.IO as IOText (readFile)

-------------------------------------------------------------------------------

data Mode = INFO
          | DEBUG
  deriving (Show)

-------------------------------------------------------------------------------

mkNode :: String -> String -> IO (Maybe LocalNode)
mkNode host port = do
  info "creating transport"
  mTransport <- createTransport host port defaultTCPParameters
  case mTransport of
    Left err -> do
      info $ show err
      return Nothing
    Right transport -> do
      node <- newLocalNode transport (remoteTable initRemoteTable)
      return $ Just node

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

out :: Mode -> String -> IO ()
out m s = putStrLn $ "[" ++ show m ++ "] " ++ s

info :: String -> IO ()
info = out INFO

debug :: String -> IO ()
debug = out DEBUG

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("help":_) -> help

    ["master", host, port] -> do
      mNode <- mkNode host port
      case mNode of
        Nothing   -> info "can't create node"
        Just node -> info "starting master" >> runProcess node runMaster

    ["node", host, port, masterHost, masterPort, workerCount] -> do
      mNode <- mkNode host port
      case mNode of
        Nothing   -> info "can't create node"
        Just node -> info "starting node" >> runProcess node (runNode masterHost masterPort (read workerCount :: Int))
    
    ["client", host, port, problem, masterHost, masterPort] -> do
      mNode <- mkNode host port
      case mNode of
        Nothing   -> info "can't create node"
        Just node -> do
          mvar <- newEmptyMVar
          runProcess node $ solveRequest masterHost masterPort (Problem TSP (Instance $ pack problem)) mvar
          ticket <- takeMVar mvar
          putStrLn . show $ ticket

    ["cli", host, port, queenHost, queenPort, filepath] -> do
      mNode <- mkNode host port
      case mNode of
        Nothing   -> info "can't create node"
        Just node -> do
          mvar <- newEmptyMVar
          
          fileContent <- IOText.readFile filepath
          
          start       <- getCurrentTime
          runProcess node $ solveRequest queenHost queenPort (Problem SSSP (Instance fileContent)) mvar
          _ticket <- takeMVar mvar
          end         <- getCurrentTime

          putStrLn $ "This took " ++ show (diffUTCTime end start)
      
    other ->
      putStrLn $ "Your arguments are invalid: " ++ show other
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}

import Control.Distributed.Process
import Control.Distributed.Process.Node hiding (newLocalNode)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet

import Data.Typeable
import GHC.Generics
import Data.Binary
import Data.List
import Control.Monad
import Control.Concurrent (threadDelay)
import System.Environment


newtype Ping = Ping ProcessId
  deriving (Typeable, Generic)

instance Binary Ping

ping :: Process ()
ping = forever $ do
  self <- getSelfPid
  Ping partner <- expect
  send partner (Ping self)
  say "ping!"
  liftIO $ threadDelay (1 * 10^6)

remotable ['ping]

runPingers :: NodeId -> Process ()
runPingers peernode = do
  ping1  <- spawnLocal ping
  ping2  <- spawn peernode $(mkStaticClosure 'ping) -- remote!
  send ping1 (Ping ping2)

myRemoteTable :: RemoteTable
myRemoteTable = __remoteTable initRemoteTable

main :: IO ()
main = do
  [port]   <- getArgs
  backend  <- initializeBackend "127.0.0.1" port myRemoteTable
  ournode  <- newLocalNode backend
  allnodes <- findPeers backend 100000
  let peernodes = delete (localNodeId ournode) allnodes
  runProcess ournode (mapM_ runPingers peernodes >> waitForever)

waitForever :: Process a
waitForever = do () <- expect
                 fail "unreachable"


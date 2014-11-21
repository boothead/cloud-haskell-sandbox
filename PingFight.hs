{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

import Data.Typeable
import GHC.Generics
import Data.Binary
import Control.Monad
import Control.Concurrent (threadDelay)


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

pingFight :: Process ()
pingFight = do
  ping1  <- spawnLocal ping
  ping2  <- spawnLocal ping
  send ping1 (Ping ping2)

main :: IO ()
main = do
    Right t  <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node     <- newLocalNode t initRemoteTable
    runProcess node (pingFight >> waitForever)
  where
    -- trick to wait forever
    waitForever = do () <- expect 
                     fail "unreachable"
    -- pingFight would finish immediately and so our whole program would shut
    -- down, so we use expect here to wait for a message that never arrives


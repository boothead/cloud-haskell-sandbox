{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}

import Control.Distributed.Process
import Control.Distributed.Process.Node hiding (newLocalNode)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet

import Control.Concurrent
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char
import qualified Data.Map as Map
import Data.Typeable
import GHC.Generics
import Data.Binary
import System.IO
import System.Environment
import System.FilePath
import Text.Read (readMaybe)


data FileServeMsg = OpenFile  ProcessId FilePath
                  | CloseFile ProcessId FilePath
                  | ReadFile  ProcessId FilePath Int Int
                  | WriteFile ProcessId FilePath Int BS.ByteString
  deriving (Typeable, Generic)
instance Binary FileServeMsg

data FileClientMsg = OpenGranted  FilePath
                   | OpenDenied   FilePath
                   | CloseConfirm FilePath
                   | ReadResult   FilePath BS.ByteString
                   | WriteConfirm FilePath
  deriving (Typeable, Generic)
instance Binary FileClientMsg

fileServer :: FilePath -> Process ()
fileServer root = go Map.empty
  where
    go :: Map.Map FilePath Handle -> Process ()
    go openfiles = do
      cmd <- expect
      case cmd of
        OpenFile client file
          | file `Map.member` openfiles ->
              send client (OpenDenied file)
          | otherwise -> do
              hnd <- liftIO $ openBinaryFile (root </> file) ReadWriteMode  
              send client (OpenGranted file)
              go (Map.insert file hnd openfiles)

        CloseFile client file
          | Just hnd <- Map.lookup file openfiles -> do
              liftIO $ hClose hnd
              send client (CloseConfirm file)
              go (Map.delete file openfiles)

        ReadFile client file offset len
          | Just hnd <- Map.lookup file openfiles -> do
              fdata <- liftIO $ do hSeek hnd AbsoluteSeek (fromIntegral offset)
                                   BS.hGet hnd len
              send client (ReadResult file fdata)
              go openfiles

        WriteFile client file offset fdata
          | Just hnd <- Map.lookup file openfiles -> do
              liftIO $ do hSeek hnd AbsoluteSeek (fromIntegral offset)
                          BS.hPut hnd fdata
              send client (WriteConfirm file)
              go openfiles

serverOpenFile :: ProcessId -> FilePath -> Process ()
serverOpenFile server file = do
    us <- getSelfPid
    send server (OpenFile us file)
    OpenGranted _ <- expect
    return ()

serverCloseFile :: ProcessId -> FilePath -> Process ()
serverCloseFile server file = do
    us <- getSelfPid
    send server (CloseFile us file)
    CloseConfirm _ <- expect
    return ()

serverReadFile :: ProcessId -> FilePath -> Int -> Int -> Process BS.ByteString
serverReadFile server file offset len = do
    us <- getSelfPid
    send server (ReadFile us file offset len)
    ReadResult _ fdata <- expect
    return fdata

serverWriteFile :: ProcessId -> FilePath -> Int -> BS.ByteString -> Process ()
serverWriteFile server file offset fdata = do
    us <- getSelfPid
    send server (WriteFile us file offset fdata)
    WriteConfirm _ <- expect
    return ()

myRemoteTable :: RemoteTable
myRemoteTable = initRemoteTable

main = do
  [mode, port]   <- getArgs
  backend  <- initializeBackend "127.0.0.1" port myRemoteTable
  ournode  <- newLocalNode backend
  case mode of
    "server" -> runServerMode ournode
    "client" -> do peers <- findPeers backend 1000000
                   runClientMode ournode peers

runServerMode :: LocalNode -> IO ()
runServerMode ournode =
    runProcess ournode $ do
      server <- getSelfPid
      register "file-server" server
      fileServer "."

runClientMode :: LocalNode -> [NodeId] -> IO ()
runClientMode ournode peers =
    runProcess ournode $ do
      server <- findServer peers
      interactiveLoop server
  where
    findServer []           = die "cannot find file server"
    findServer (peer:peers) = do
      whereisRemoteAsync peer "file-server"
      WhereIsReply _ mpid <- expect
      case mpid of
        Just serverpid -> return serverpid
        Nothing        -> findServer peers

interactiveLoop :: ProcessId -> Process ()
interactiveLoop server = do
    liftIO $ do putStr "\nenter command> "
                hFlush stdout
    ln <- liftIO getLine
    case words ln of
      ["open",  file] -> do
        serverOpenFile server file
        interactiveLoop server

      ["close", file] -> do
        serverCloseFile server file
        interactiveLoop server

      ["read", file, offstr, lenstr]
        | Just off <- readMaybe offstr
        , Just len <- readMaybe lenstr -> do
            fdata <- serverReadFile server file off len
            liftIO $ BS.Char.putStrLn fdata
            interactiveLoop server

      ["write", file, offstr, fdata]
        | Just off <- readMaybe offstr -> do
            serverWriteFile server file off (BS.Char.pack fdata)
            interactiveLoop server

      ["exit"] -> return ()
      
      _ -> do liftIO $ putStrLn "unrecognised command"
              interactiveLoop server


module Server where

import           AsyncAPI               (race)
import           Control.Concurrent     (forkFinally)
import           Control.Concurrent.STM
import qualified Control.Exception      as E
import           Control.Monad
import           Data.Maybe             (fromMaybe)
import           Network.Socket
import           System.IO
import           Text.Printf

type Port = Int

newtype State = State { currentFactor :: TVar Int }

withServer :: IO ()
withServer = do
  factor <- atomically $ newTVar 2
  runTCPServer Nothing 44444 consume
    where
      consume sock = do
        factor <- atomically $ newTVar 2
        handle <- socketToHandle sock ReadWriteMode
        talk handle factor

runTCPServer :: Maybe HostName -> Port -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    host = fromMaybe "127.0.0.1" mhost
    resolve = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE]
                               , addrSocketType = Stream
                               }
      printf "Listening on port %d\n" port
      head <$> getAddrInfo (Just hints) (Just host) (Just $ show port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      let fd = fdSocket sock
      setCloseOnExecIfNeeded fd
      bind sock $ addrAddress addr
      listen sock 1024
      printf "Accepted connection from %s:%d\n" host port
      return sock
    loop sock = forever $ do
      (conn, _peer) <- accept sock
      void $ forkFinally (server conn) (\_ -> close conn)

talk :: Handle -> TVar Int -> IO ()
talk handle factor = do
  hSetBuffering handle LineBuffering
  chan <- atomically newTChan
  race (server handle factor chan) (receive handle chan)
  return ()

receive :: Handle -> TChan String -> IO ()
receive handle chan = forever $ do
  line <- hGetLine handle
  atomically $ writeTChan chan line

server :: Handle -> TVar Int -> TChan String -> IO ()
server handle factor chan = do
  f <- atomically $ readTVar factor
  hPrintf handle "Current factor: %d\n" f
  loop f
    where
      loop f = do
        action <- atomically $ do
          f' <- readTVar factor
          if (f /= f')
             then return (newfactor f')
             else do
               l <- readTChan chan
               return (command f l)
        action

      newfactor f = do
        hPrintf handle "new factor: %d\n" f
        loop f

      command f s
        = case s of
            "end" -> hPutStrLn handle "Thank you for using the Haskell multiplying service."
            '*':s -> do
              atomically $ writeTVar factor (read s :: Int)
              loop f
            line -> do
              hPutStrLn handle (show (f * (read line :: Int)))
              loop f


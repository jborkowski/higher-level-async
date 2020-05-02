{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Chat where

import           AsyncAPI               (race)
import           Control.Concurrent     (forkFinally)
import           Control.Concurrent.STM
import qualified Control.Exception      as E
import           Control.Monad
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import           Network.Socket         hiding (Broadcast)
import           System.IO
import           Text.Printf

type Port = Int

type ClientName = String

data Client = Client
  { clientName     :: ClientName
  , clientHandle   :: Handle
  , clientKicked   :: TVar (Maybe String)
  , clientSendChan :: TChan Message
  }

data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command String

newtype Server = Server
  { clients :: TVar (M.Map ClientName Client)
  }

newServer :: IO Server
newServer = do
  c <- newTVarIO M.empty
  return Server { clients = c }

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientmap <- readTVar clients
  mapM_ (`sendMessage` msg) (M.elems clientmap)

withServer :: IO ()
withServer =
  runTCPServer host port consume
    where
      port = 44444
      host = Nothing
      consume sock = do
        server <- newServer
        handle <- socketToHandle sock ReadWriteMode
        talk handle server

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

checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar clients
  if M.member name clientmap
     then return Nothing
     else do client <- newClient name handle
             writeTVar clients $ M.insert name client clientmap
             broadcast server $ Notice (name ++ " has connected")
             return (Just client)

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar' clients $ M.delete name
  broadcast server $ Notice (name ++ " has disconnected")

talk :: Handle -> Server -> IO ()
talk handle server = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
    where
      readName = do
        hPutStrLn handle "What is your name?"
        name <- hGetLine handle
        if null name
          then readName
          else E.mask $ \restore ->
            checkAddClient server name handle >>= \case
          Nothing -> do
            hPrintf handle "The name %s is in use, please choose another\n" name
            readName
          Just client ->
            E.finally (restore (runClient server client)) (removeClient server name)

receive :: Handle -> TChan String -> IO ()
receive handle chan = forever $ do
  line <- hGetLine handle
  atomically $ writeTChan chan line

newClient name handle = do
  chan   <- newTChan
  kicked <- newTVar Nothing
  return Client { clientName     = name
                , clientHandle   = handle
                , clientSendChan = chan
                , clientKicked   = kicked
                }

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} = writeTChan clientSendChan

sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server{..} name msg = do
  clientmap <- readTVar clients
  case M.lookup name clientmap of
    Nothing     -> return False
    Just client -> sendMessage client msg >> return True

runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  race server receive
  return ()
    where
      receive = forever $ do
        msg <- hGetLine clientHandle
        atomically $ sendMessage client (Command msg)

      server = join $ atomically $ readTVar clientKicked >>= \case
        Just reason -> return $ hPutStrLn clientHandle $ "You have been kicked: " ++ reason
        Nothing -> do
          msg <- readTChan clientSendChan
          return $ do
            continue <- handleMessage serv client msg
            when continue server

tell :: Server -> Client -> ClientName -> String -> IO ()
tell server@Server{..} Client{..} who msg = do
  ok <- atomically $ sendToName server who (Tell clientName msg)
  if ok
     then return ()
     else hPutStrLn clientHandle (who ++ " is not connected.")

kick :: Server -> ClientName -> ClientName -> STM ()
kick server@Server{..} who by = do
  clientmap <- readTVar clients
  case M.lookup who clientmap of
    Nothing ->
      void $ sendToName server by (Notice $ who ++ " is not connected")
    Just victim -> do
      writeTVar (clientKicked victim) $ Just ("by " ++ by)
      void $ sendToName server by (Notice $ "you kicked " ++ who)

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
    Notice msg         -> output $ "*** " ++ msg
    Tell name msg      -> output $ "*" ++ name ++ "*" ++ msg
    Broadcast name msg -> output $ "<" ++ name ++ ">" ++ msg
    Command msg -> case words msg of
      ["/kick", who] -> do
        atomically $ kick server who clientName
        return True
      "/tell" : who : what -> do
        tell server client who (unwords what)
        return True
      ["/quit"] ->
        return False
      ('/':_):_ -> do
        hPutStrLn clientHandle $ "Unrecognized command: " ++ msg
        return True
      _ -> do
        atomically $ broadcast server $ Broadcast clientName msg
        return True
    where
      output s = do hPutStrLn clientHandle s; return True

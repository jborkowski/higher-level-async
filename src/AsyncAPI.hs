module AsyncAPI where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

forkFinally' :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally' action andThen =
  mask $ \restore ->
    forkIO $ try (restore action) >>= andThen

data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO
  t <- forkFinally action (atomically . putTMVar var)
  return (Async t var)

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ var) = readTMVar var

waitSTM :: Async a -> STM a
waitSTM a = do
  r <- waitCatchSTM a
  case r of
    Left e -> throwSTM e
    Right a -> return a

wait :: Async a -> IO a
wait = atomically . waitSTM

cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled

withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync io = bracket (async io) cancel

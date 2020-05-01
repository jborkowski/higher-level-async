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

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $
  fmap Left (waitSTM a)
  `orElse`
  fmap Right (waitSTM b)

waitBoth :: Async a -> Async b -> IO (a, b)
waitBoth a b = atomically $ do
  ra <- waitSTM a `orElse` (do waitSTM b; retry)
  rb <- waitSTM b
  return (ra, rb)

concurrently :: IO a -> IO b -> IO (a, b)
concurrently ioa iob =
  withAsync ioa $ \a ->
  withAsync iob $ \b ->
    waitBoth a b


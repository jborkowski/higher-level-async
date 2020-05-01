{-# LANGUAGE LambdaCase #-}
module AsyncAPI where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Functor

forkFinally' :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally' action andThen =
  mask $ \restore ->
    forkIO $ try (restore action) >>= andThen

data Async a = Async ThreadId (STM (Either SomeException a))

instance Functor Async where
  fmap f (Async t stm) = Async t stm'
    where
      stm' = stm >>= \case
        Left e  -> return (Left e)
        Right a -> return (Right (f a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO
  t <- forkFinally action (atomically . putTMVar var)
  return (Async t (readTMVar var))

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ stm) = stm

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

race :: IO a -> IO b -> IO (Either a b)
race ioa iob =
  withAsync ioa $ \a ->
  withAsync iob $ \b ->
    waitEither a b

timeout :: Int -> IO a -> IO (Maybe a)
timeout n ioa
  | n < 0     = fmap Just ioa
  | n == 0    = return Nothing
  | otherwise =
     race (threadDelay n) ioa >>= \case
        Left _ -> return Nothing
        Right a -> return (Just a)

waitAny :: [Async a] -> IO a
waitAny asyncs = atomically $ foldr orElse retry $ asyncs <&> waitSTM

module SlaveThread
(
  fork,
  forkFinally,
)
where

import BasePrelude hiding (forkFinally)
import Control.Monad.Trans.Reader
import Control.Monad.Morph
import qualified STMContainers.Multimap as Multimap
import qualified PartialHandler
import qualified ListT


-- |
-- A global registry of all slave threads by their masters.
slaves :: Multimap.Multimap ThreadId ThreadId
slaves =
  unsafePerformIO $ Multimap.newIO

-- |
-- Fork a slave thread to run a computation on.
fork :: IO a -> IO ThreadId
fork main =
  forkFinally (return ()) main

-- |
-- Fork a slave thread with a finalizer action to run a computation on.
-- The finalizer gets executed when the thread dies for whatever reason:
-- due to being killed or an uncaught exception, or a normal termination.
-- 
-- Note the order of arguments:
-- 
-- >forkFinally finalizer computation
forkFinally :: IO a -> IO b -> IO ThreadId
forkFinally finalizer computation =
  do
    masterThread <- myThreadId
    -- Ensures that the thread gets registered before this function returns.
    semaphore <- newEmptyMVar
    slaveThread <-
      mask $ \restore -> forkIO $ do
        slaveThread <- myThreadId
        atomically $ Multimap.insert slaveThread masterThread slaves
        putMVar semaphore ()
        r <- try $ restore computation
        -- Context management:
        killSlaves slaveThread
        waitForSlavesToDie slaveThread
        -- Finalization and rethrowing of exceptions into the master thread:
        forM_ (left r) $ 
          PartialHandler.totalizeRethrowingTo_ masterThread $ 
            PartialHandler.onThreadKilled (return ())
        try finalizer >>= \r ->
          forM_ (left r) $ PartialHandler.totalizeRethrowingTo_ masterThread $ mempty
        -- Unregister from the global state,
        -- thus informing the master of this thread's death.
        atomically $ Multimap.delete slaveThread masterThread slaves
    takeMVar semaphore
    return slaveThread
  where
    left = either Just (const Nothing)

killSlaves :: ThreadId -> IO ()
killSlaves thread =
  ListT.traverse_ killThread $ hoist atomically $ Multimap.streamByKey thread slaves

waitForSlavesToDie :: ThreadId -> IO ()
waitForSlavesToDie thread =
  atomically $ do
    null <- ListT.null $ Multimap.streamByKey thread slaves
    if null
      then return ()
      else retry

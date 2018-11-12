-- |
-- Vanilla thread management in Haskell is low level and
-- it does not approach the problems related to thread deaths.
-- When it's used naively the following typical problems arise:
--
-- * When a forked thread dies due to an uncaught exception,
-- the exception does not get raised in the main thread,
-- which is why the program continues to run as if nothing happened,
-- i.e., with the presumption that the already dead thread is running normally.
-- Naturally this may very well bring your program to a chaotic state.
--
-- * Another issue is that one thread dying does not
-- affect any of the threads forked from it.
-- That's why your program may be accumulating ghost threads.
--
-- * Ever dealt with your program ignoring the \<Ctrl-C\> strikes?
--
-- This library solves all the issues above with a concept of a slave thread.
-- A slave thread has the following properties:
--
-- 1. When it dies for whatever reason (exception or finishing normally)
-- it kills all the slave threads that were forked from it.
-- This protects you from ghost threads.
--
-- 2. It waits for all slaves to die and execute their finalizers
-- before executing its own finalizer and getting released itself.
-- This gives you hierarchical releasing of resources.
--
-- 3. When a slave thread dies with an uncaught exception
-- it reraises it in the master thread.
-- This protects you from silent exceptions
-- and lets you be sure of getting informed
-- if your program gets brought to an erroneous state.
module SlaveThread
(
  fork,
  forkFinally,
)
where

import SlaveThread.Prelude
import SlaveThread.Util.LowLevelForking
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import qualified StmContainers.Multimap as Multimap
import qualified Control.Foldl as Foldl


-- |
-- A global registry of all slave threads by their masters.
{-# NOINLINE slaveRegistry #-}
slaveRegistry :: Multimap.Multimap ThreadId ThreadId
slaveRegistry =
  unsafePerformIO Multimap.newIO

-- |
-- Fork a slave thread to run a computation on.
{-# INLINABLE fork #-}
fork :: IO a -> IO ThreadId
fork =
  forkFinally $ return ()

-- |
-- Fork a slave thread with a finalizer action to run a computation on.
-- The finalizer gets executed when the thread dies for whatever reason:
-- due to being killed or an uncaught exception, or a normal termination.
--
-- Note the order of arguments:
--
-- >forkFinally finalizer computation
{-# INLINABLE forkFinally #-}
forkFinally :: IO a -> IO b -> IO ThreadId
forkFinally finalizer computation =
  uninterruptibleMask_ $ do
    masterThread <- myThreadId
    -- Ensures that the thread gets registered before being unregistered
    registrationGate <- newEmptyMVar
    slaveThread <- forkIOWithUnmaskWithoutHandler $ \ unmask -> do

      slaveThread <- myThreadId

      let log message = traceM (show slaveThread <> ": " <> message)

      log ("Forking from " <> show masterThread)

      -- Execute the main computation:
      computationExceptionIfAny <- catch (unmask computation $> Nothing) (return . Just)

      -- Kill the slaves and wait for them to die:      
      killSlaves slaveThread
      slavesDyingExceptions <- let
        loop !exceptions =
          catch
            (unmask (waitForSlavesToDie slaveThread) $> exceptions)
            (\ !exception -> loop (exception : exceptions))
          in loop []

      log (show slavesDyingExceptions)

      log "Finalizing"
      -- Finalize:
      finalizerExceptionIfAny <- catch (finalizer $> Nothing) (return . Just)

      log "Waiting at registration gate"
      -- Unregister from the global state,
      -- thus informing the master of this thread's death:
      takeMVar registrationGate
      log "Deleting itself from the registry"
      atomically $ Multimap.delete slaveThread masterThread slaveRegistry

      log "Processing the exceptions"
      -- Process the exceptions:
      let
        handler e = case fromException e of
          Just ThreadKilled -> return ()
          _ -> throwTo masterThread e
        in do
          log "Processing the computation exception"
          forM_ computationExceptionIfAny handler
          log "Processing the slaves dying exceptions"
          forM_ slavesDyingExceptions handler
          log "Processing the finalizer exceptions"
          forM_ finalizerExceptionIfAny handler

      log "Finishing"

    atomically $ Multimap.insert slaveThread masterThread slaveRegistry
    putMVar registrationGate ()
    return slaveThread

killSlaves :: ThreadId -> IO ()
killSlaves thread = do
  threads <- atomically (UnfoldlM.foldM (Foldl.generalize Foldl.revList) (Multimap.unfoldMByKey thread slaveRegistry))
  traverse_ killThread threads

waitForSlavesToDie :: ThreadId -> IO ()
waitForSlavesToDie thread =
  atomically $ do
    null <- UnfoldlM.null $ Multimap.unfoldMByKey thread slaveRegistry
    unless null retry

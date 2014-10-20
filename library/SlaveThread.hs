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

import BasePrelude hiding (forkFinally)
import Control.Monad.Trans.Reader
import Control.Monad.Morph
import GHC.IO (IO(IO))
import GHC.Exts (Int(I#), fork#, forkOn#)
import qualified STMContainers.Multimap as Multimap
import qualified PartialHandler
import qualified ListT


-- |
-- A global registry of all slave threads by their masters.
{-# NOINLINE slaves #-}
slaves :: Multimap.Multimap ThreadId ThreadId
slaves =
  unsafePerformIO Multimap.newIO

-- |
-- Fork a slave thread to run a computation on.
{-# INLINABLE fork #-}
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
{-# INLINABLE forkFinally #-}
forkFinally :: IO a -> IO b -> IO ThreadId
forkFinally finalizer computation =
  mask $ \unmask -> do
    masterThread <- myThreadId
    -- Ensures that the thread gets registered before executing the forked
    -- computation
    gate <- newEmptyMVar
    slaveThread <-
      forkIOWithoutHandler $ do
        slaveThread <- myThreadId
        computationException <- fmap left $ try $ unmask computation
        -- Context management:
        killSlaves slaveThread
        waitForSlavesToDie slaveThread
        -- Finalization:
        finalizerException <- fmap left $ try $ finalizer
        -- Rethrowing of exceptions into the master thread:
        forM_ (computationException <|> finalizerException) $ 
          PartialHandler.totalizeRethrowingTo_ masterThread $ 
            PartialHandler.onThreadKilled (return ())
        -- Unregister from the global state,
        -- thus informing the master of this thread's death.
        takeMVar gate
        atomically $ Multimap.delete slaveThread masterThread slaves
    atomically $ Multimap.insert slaveThread masterThread slaves
    putMVar gate ()
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
    when (not null) retry

-- |
-- A more efficient version of 'forkIO', 
-- which does not install a default exception handler on the forked thread.
{-# INLINE forkIOWithoutHandler #-}
forkIOWithoutHandler :: IO () -> IO ThreadId
forkIOWithoutHandler action = 
  IO $ \s -> case (fork# action s) of (# s', tid #) -> (# s', ThreadId tid #)

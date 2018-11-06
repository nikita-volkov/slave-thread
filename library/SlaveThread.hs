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

import Prelude
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Concurrent hiding (forkFinally)
import Control.Exception
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Reader
import GHC.Conc
import GHC.Exts (Int(I#), fork#, forkOn#)
import GHC.IO (IO(IO))
import System.IO.Unsafe
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import qualified Focus
import qualified PartialHandler
import qualified StmContainers.Map as Map
import qualified StmContainers.Multimap as Multimap
import qualified Control.Foldl as Foldl


-- |
-- A global registry of all slave threads by their masters.
{-# NOINLINE slaves #-}
slaves :: Multimap.Multimap ThreadId ThreadId
slaves =
  unsafePerformIO Multimap.newIO

-- |
-- A global registry of all master threads by their slaves.
{-# NOINLINE masters #-}
masters :: Map.Map ThreadId ThreadId
masters =
  unsafePerformIO Map.newIO

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
  mask $ \unmask -> do
    masterThread <- myThreadId
    -- Ensures that the thread gets registered before being unregistered
    registrationGate <- newEmptyMVar
    slaveThread <-
      forkIOWithoutHandler $ do
        slaveThread <- myThreadId
        catch
          (unmask $ do
            computation
            -- Context management:
            killSlaves slaveThread
            waitForSlavesToDie slaveThread)
          (PartialHandler.totalizeRethrowingTo_ masterThread
            (PartialHandler.onThreadKilled (return ())))
        -- Finalization:
        catch (void finalizer) $
          PartialHandler.totalizeRethrowingTo_ masterThread $ mempty
        -- Unregister from the global state,
        -- thus informing the master of this thread's death:
        takeMVar registrationGate
        atomically $ do
          Multimap.delete slaveThread masterThread slaves
          Map.delete masterThread masters
    atomically $ do
      Multimap.insert slaveThread masterThread slaves
      Map.insert masterThread slaveThread masters
    putMVar registrationGate ()
    return slaveThread

killSlaves :: ThreadId -> IO ()
killSlaves thread = do
  threads <- atomically (UnfoldlM.foldM (Foldl.generalize Foldl.revList) (Multimap.unfoldMByKey thread slaves))
  traverse_ killThread threads

waitForSlavesToDie :: ThreadId -> IO ()
waitForSlavesToDie thread =
  atomically $ do
    null <- UnfoldlM.null $ Multimap.unfoldMByKey thread slaves
    unless null retry

-- |
-- Link the current thread to the given master thread. After this operation,
-- it's as if the given 'ThreadId' 'fork'ed the current thread, even if the
-- current thread originally had a different master.
--
-- This function can be used to dynamically reconfigure the thread hierarchy,
-- but its primary use is for interop with other threded libraries that were not
-- designed with @slave-thread@ in mind.
link :: ThreadId -> IO ()
link newMasterThread = do
  slaveThread <- myThreadId
  unless (newMasterThread == slaveThread) $
    atomically $ do
      Map.focus (Focus.lookup <* Focus.insert newMasterThread) slaveThread masters >>= \case
        Nothing -> Multimap.insert slaveThread newMasterThread slaves
        Just oldMasterThread
          | newMasterThread == oldMasterThread -> return ()
          | otherwise -> do
              Multimap.delete slaveThread oldMasterThread slaves
              Multimap.insert slaveThread newMasterThread slaves

-- |
-- A more efficient version of 'forkIO',
-- which does not install a default exception handler on the forked thread.
{-# INLINE forkIOWithoutHandler #-}
forkIOWithoutHandler :: IO () -> IO ThreadId
forkIOWithoutHandler action = 
  IO $ \s -> case (fork# action s) of (# s', tid #) -> (# s', ThreadId tid #)

{-# LANGUAGE CPP #-}
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
  forkWithUnmask,
  forkFinally,
  forkFinallyWithUnmask,
  SlaveThreadCrashed(..)
  -- * Notes
  -- $note-unmask
)
where

import SlaveThread.Prelude
import SlaveThread.Util.LowLevelForking
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import qualified StmContainers.Multimap as Multimap
import qualified Control.Foldl as Foldl
import qualified Focus


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
-- Like 'fork', but provides the computation a function that unmasks
-- asynchronous exceptions. See @Note [Unmask]@ at the bottom of this module.
{-# INLINABLE forkWithUnmask #-}
forkWithUnmask :: ((forall x. IO x -> IO x) -> IO a) -> IO ThreadId
forkWithUnmask =
  forkFinallyWithUnmask $ return ()

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
  forkFinallyWithUnmask finalizer (\unmask -> unmask computation)

-- |
-- Like 'forkFinally', but provides the computation a function that unmasks
-- asynchronous exceptions. See @Note [Unmask]@ at the bottom of this module.
{-# INLINABLE forkFinallyWithUnmask #-}
forkFinallyWithUnmask :: IO a -> ((forall x. IO x -> IO x) -> IO b) -> IO ThreadId
forkFinallyWithUnmask finalizer computation =
  uninterruptibleMask $ \unmask -> do

    masterThread <- myThreadId

    slaveThread <- forkIOWithoutHandler $ do

      slaveThread <- myThreadId

      -- Execute the main computation:
      computationExceptions <- catch (computation unmask $> empty) (return . pure)

      -- Kill the slaves and wait for them to die:
      slavesDyingExceptions <- let
        loop !exceptions =
          catch
            (unmask $ do
              killSlaves slaveThread
              waitForSlavesToDie slaveThread
              return exceptions)
            (\ !exception -> loop (exception : exceptions))
          in loop []

      -- Finalize:
      finalizerExceptions <- catch (finalizer $> empty) (return . pure)

      -- Rethrow the exceptions:
      let
        handler e = do
          case fromException e of
            Just ThreadKilled -> return ()
            _ -> throwTo masterThread (SlaveThreadCrashed slaveThread e)
        in do
          forM_ @Maybe computationExceptions handler
          forM_ slavesDyingExceptions handler
          forM_ @Maybe finalizerExceptions handler

      -- Unregister from the global state,
      -- thus informing the master of this thread's death.
      -- Whilst doing so, also ensure that the master has already registered this slave.
      atomically $ do
        result <- Multimap.focus Focus.lookupAndDelete slaveThread masterThread slaveRegistry
        case result of
          Just _ -> return ()
          _ -> retry

    atomically $ Multimap.insert slaveThread masterThread slaveRegistry

    return slaveThread

killSlaves :: ThreadId -> IO ()
killSlaves thread = do
#if MIN_VERSION_stm_containers(1,2,0)
  threads <- atomically (UnfoldlM.foldM (Foldl.generalize Foldl.revList) (Multimap.unfoldlMByKey thread slaveRegistry))
#else
  threads <- atomically (UnfoldlM.foldM (Foldl.generalize Foldl.revList) (Multimap.unfoldMByKey thread slaveRegistry))
#endif
  traverse_ killThread threads

waitForSlavesToDie :: ThreadId -> IO ()
waitForSlavesToDie thread =
  atomically $ do
#if MIN_VERSION_stm_containers(1,2,0)
    null <- UnfoldlM.null $ Multimap.unfoldlMByKey thread slaveRegistry
#else
    null <- UnfoldlM.null $ Multimap.unfoldMByKey thread slaveRegistry
#endif
    unless null retry

-- | A slave thread crashed. This exception is classified as /asynchronous/,
-- meaning it extends from 'SomeAsyncException'.
--
-- In general,
--
-- * /Synchronous/ exceptions such as 'IOException' are thrown by IO actions
--   that are explicitly called by the thread that receives them, and may be
--   caught, inspected, and handled by resuming execution.
-- * /Asynchronous/ exceptions such as 'ThreadKilled' should normally only be
--   caught temporarily in order to run finalizers, then re-thrown.
--
-- 'SlaveThreadCrashed' being asynchronous means it should, by default, cause
-- the entire thread hierarchy to come crashing down, ultimately terminating the
-- program.
--
-- If you want more sophisticated behavior, such as a "supervisor" thread that
-- monitors and restarts worker threads when they fail, you have to program
-- that yourself.
--
-- N.B. Consider using a library like
-- @<https://hackage.haskell.org/package/safe-exceptions safe-exceptions>@ or
-- @<https://hackage.haskell.org/package/unliftio unliftio>@, which carefully
-- distinguish synchronous and asynchronous exceptions, unlike @base@.
data SlaveThreadCrashed
  = SlaveThreadCrashed !ThreadId !SomeException
  deriving (Show)

instance Exception SlaveThreadCrashed where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- $note-unmask
--
-- == Masking
--
-- Threads forked by this library, unlike in @base@, /already/ mask asynchronous
-- exceptions internally, for bookkeeping purposes.
--
-- The @*withUnmask@ variants of 'fork' are thus different from the
-- @*withUnmask@ variants found in @base@ and @async@, in that the unmasking
-- function they provide restores the masking state /to that of the calling context/,
-- as opposed to /unmasked/.
--
-- Put another way, the @base@ code that you may have written as:
--
-- @
-- mask (\\unmask -> forkIO (initialize >> unmask computation))
-- @
--
-- using this library would be instead written as:
--
-- @
-- 'forkWithUnmask' (\\unmask -> initialize >> unmask computation)
-- @
--
-- And the @base@ code that you may have written as:
--
-- @
-- mask_ (forkIOWithUnmask (\\unmask -> initialize >> unmask computation))
-- @
--
-- will instead have to /manually/ call the low-level unmasking function called
-- 'GHC.IO.unsafeUnmask', as:
--
-- @
-- mask_ ('forkWithUnmask' (\\_ -> initialize >> unsafeUnmask computation))
-- @
--
-- Note that we used 'forkWithUnmask' (to guarantee @initialize@ is run with
-- asynchronous exceptions masked), but the unmasking function it provided does
-- not guarantee asynchronous exceptions are actually unmasked, so we toss it
-- and use 'GHC.IO.unsafeUnmask' instead.
--
-- This idiom is uncommon, but necessary when you need to fork a thread in
-- library code that is unsure if it's being called with asynchronous exceptions
-- masked (as in the "acquire" phase of a @bracket@ call).

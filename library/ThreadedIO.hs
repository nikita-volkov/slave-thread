module ThreadedIO where

import BasePrelude hiding (forkFinally)
import Control.Monad.Trans.Reader
import qualified BasePrelude
import qualified STMContainers.Set as Set
import qualified PartialHandler

-- |
-- A monad, which adds a functionality of forking of slave threads,
-- while binding them to their master thread in such a manner
-- that when the master is killed, they get killed aswell.
-- It also rethrows exceptions from the slave threads in the main thread,
-- so they don't get lost.
type ThreadedIO =
  ReaderT (Set.Set ThreadId) IO

-- |
-- A convenience alias of 'ThreadedIO'.
type TIO = 
  ThreadedIO

run :: TIO a -> IO a
run mt =
  do
    context <- atomically $ Set.new
    catch (runReaderT mt context) $ \(e :: SomeException) -> do
      -- Kill all slaves
      traverse_ killThread =<< do
        atomically $ Set.foldM (\l -> return . (: l)) [] context
      -- Wait for all slaves to die
      atomically $ Set.null context >>= bool retry (return ())
      throwIO e

-- | 
-- Run the 'ThreadedIO' monad, which performs no subforking.
runWithoutForking :: TIO a -> IO a
runWithoutForking mt =
  runReaderT mt (error "Attempt to fork when executed with 'runWithoutForking'")

forkFinally :: TIO () -> IO () -> TIO ThreadId
forkFinally main finalizer =
  ReaderT $ \context -> do
    thread <- myThreadId
    slaveContext <- atomically $ Set.new
    let
      onDeath r =
        do
          -- Finalization and rethrowing of exceptions into the master thread:
          do
            r' <- try $ finalizer
            forM_ (left r <|> left r') $ 
              PartialHandler.totalizeRethrowingTo_ thread $ 
                PartialHandler.onThreadKilled (return ())
          -- Context management:
          do
            traverse_ killThread =<< do
              atomically $ Set.foldM (\l -> return . (: l)) [] slaveContext
            slaveThread <- myThreadId
            -- Ensures that it waits for all slaves to die 
            -- before informing the master that it died itself.
            -- And so on recursively.
            atomically $ do
              Set.null slaveContext >>= \case
                True -> Set.delete slaveThread context
                False -> retry
        where
          left = either Just (const Nothing)
    slaveThread <- BasePrelude.forkFinally (runReaderT main slaveContext) onDeath
    atomically $ Set.insert slaveThread context
    return slaveThread

fork :: TIO () -> TIO ThreadId
fork main =
  forkFinally main (return ())






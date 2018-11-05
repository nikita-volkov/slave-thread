{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Prelude
import Test.Framework
import Test.QuickCheck.Instances
import qualified SlaveThread as S
import qualified Control.Concurrent.SSem as SSem


main = 
  htfMain $ htf_thisModulesTests


test_failingInFinalizerDoesntBreakEverything =
  do
    finalizer1CalledVar <- newTVarIO False
    finalizer2CalledVar <- newTVarIO False
    let
      finalizer1 =
        atomically $ writeTVar finalizer1CalledVar True
      finalizer2 =
        do
          atomically $ writeTVar finalizer2CalledVar True
          throwIO (userError "finalizer2 failed")
      in do
        S.forkFinally finalizer1 $ do
          S.forkFinally finalizer2 $ threadDelay 1000
        threadDelay (10^6)
        finalizer2Called <- atomically (readTVar finalizer2CalledVar)
        finalizer1Called <- atomically (readTVar finalizer1CalledVar)
        assertEqual True finalizer2Called
        assertEqual True finalizer1Called

test_forkedThreadsRunFine = 
  replicateM 100000 $ do
    var <- newMVar 0
    let increment = modifyMVar_ var (return . succ)
    semaphore <- SSem.new 0
    S.fork $ do
      increment
      semaphore' <- SSem.new (-1)
      S.fork $ do
        increment
        SSem.signal semaphore'
      S.fork $ do
        increment
        SSem.signal semaphore'
      SSem.wait semaphore'
      SSem.signal semaphore
    SSem.wait semaphore
    assertEqual 3 =<< readMVar var

test_killingAThreadKillsDeepSlaves = 
  replicateM 100000 $ do
    var <- newMVar 0
    semaphore <- SSem.new 0
    thread <- 
      S.forkFinally (SSem.signal semaphore) $ do
        join $ forkWait $ do
          join $ forkWait $ do
            w <- forkWait $ do
              threadDelay $ 10^6
              modifyMVar_ var (return . succ)
            threadDelay $ 10^6
            modifyMVar_ var (return . succ)
            w
    killThread thread
    SSem.wait semaphore
    assertEqual 0 =<< readMVar var

test_dyingNormallyKillsSlaves = 
  replicateM 100000 $ do
    var <- newIORef 0
    let increment = modifyIORef var (+1)
    semaphore <- SSem.new 0
    S.forkFinally (SSem.signal semaphore) $ do
      S.fork $ do
        threadDelay $ 10^6
        increment
      S.fork $ do
        threadDelay $ 10^6
        increment
    SSem.wait semaphore
    assertEqual 0 =<< readIORef var

test_finalizationOrder = 
  replicateM 100000 $ do
    var <- newMVar []
    semaphore <- SSem.new 0
    S.forkFinally (uninterruptibleMask_ (modifyMVar_ var (return . (1:)) >> SSem.signal semaphore)) $ do
      semaphore' <- SSem.new 0
      S.forkFinally (uninterruptibleMask_ (modifyMVar_ var (return . (2:)) >> SSem.signal semaphore')) $ do
        S.forkFinally (uninterruptibleMask_ (modifyMVar_ var (return . (3:)))) $ return ()
        S.forkFinally (uninterruptibleMask_ (modifyMVar_ var (return . (3:)))) $ return ()
      SSem.wait semaphore'
    SSem.wait semaphore
    assertEqual [1,2,3,3] =<< readMVar var

test_exceptionsDontGetLost = 
  replicateM 100000 $ do
    assertThrowsSomeIO $ do
      S.fork $ do
        S.fork $ do
          error "!"
        threadDelay $ 10^6
      threadDelay $ 10^6

forkWait :: IO a -> IO (IO ())
forkWait io =
  do
    v <- newEmptyMVar
    S.fork $ do
      r <- try io
      putMVar v ()
      either (throwIO :: SomeException -> IO a) return r
    return $ takeMVar v

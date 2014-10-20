{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import BasePrelude
import Test.Framework
import Test.QuickCheck.Instances
import qualified SlaveThread as S
import qualified Control.Concurrent.SSem as SSem


main = 
  htfMain $ htf_thisModulesTests


test_forkedThreadsRunFine = 
  replicateM 100 $ do
    var <- newIORef 0
    let increment = modifyIORef var (+1)
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
    assertEqual 3 =<< readIORef var

test_killingAThreadKillsDeepSlaves = 
  replicateM 100 $ do
    var <- newIORef 0
    let increment = modifyIORef var (+1)
    semaphore <- SSem.new 0
    thread <- 
      S.forkFinally (SSem.signal semaphore) $ do
        join $ forkWait $ do
          join $ forkWait $ do
            w <- forkWait $ do
              threadDelay $ 10^6
              increment
            threadDelay $ 10^6
            increment
            w
    killThread thread
    SSem.wait semaphore
    assertEqual 0 =<< readIORef var

test_dyingNormallyKillsSlaves = 
  replicateM 100 $ do
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
  replicateM 1000 $ do
    var <- newMVar []
    semaphore <- SSem.new 0
    S.forkFinally (modifyMVar_ var (return . (1:)) >> SSem.signal semaphore) $ do
      semaphore <- SSem.new 0
      S.forkFinally (modifyMVar_ var (return . (2:)) >> SSem.signal semaphore) $ do
        S.forkFinally (modifyMVar_ var (return . (3:))) $ return ()
        S.forkFinally (modifyMVar_ var (return . (3:))) $ return ()
        S.forkFinally (modifyMVar_ var (return . (3:))) $ return ()
        S.forkFinally (modifyMVar_ var (return . (3:))) $ return ()
        S.forkFinally (modifyMVar_ var (return . (3:))) $ return ()
        S.forkFinally (modifyMVar_ var (return . (3:))) $ return ()
        S.forkFinally (modifyMVar_ var (return . (3:))) $ return ()
      SSem.wait semaphore
    SSem.wait semaphore
    assertEqual [1,2,3,3,3,3,3,3,3] =<< readMVar var

test_exceptionsDontGetLost = 
  replicateM 100 $ do
    assertThrowsSomeIO $ do
      S.fork $ do
        S.fork $ do
          error "!"
        threadDelay $ 10^4
      threadDelay $ 10^4

forkWait :: IO a -> IO (IO ())
forkWait io =
  do
    v <- newEmptyMVar
    S.fork $ do
      r <- try io
      putMVar v ()
      either (throwIO :: SomeException -> IO a) return r
    return $ takeMVar v

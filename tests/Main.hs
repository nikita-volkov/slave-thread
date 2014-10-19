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
        w <- forkWait $ do
          w <- forkWait $ do
            threadDelay $ 20*10^3
            increment
          w
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
        threadDelay $ 10^4
        increment
      S.fork $ do
        threadDelay $ 10^4
        increment
    SSem.wait semaphore
    assertEqual 0 =<< readIORef var

test_finalizationOrder = 
  replicateM 1000 $ do
    var <- newIORef 0
    semaphore <- SSem.new 0
    S.forkFinally (modifyIORef var (*2) >> SSem.signal semaphore) $ do
      S.forkFinally (modifyIORef var (+1)) $ return ()
      S.forkFinally (modifyIORef var (+1)) $ return ()
    SSem.wait semaphore
    assertEqual 4 =<< readIORef var

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

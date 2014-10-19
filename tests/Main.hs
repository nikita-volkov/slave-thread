{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import BasePrelude
import Test.Framework
import Test.QuickCheck.Instances
import qualified SlaveThread as S


main = 
  htfMain $ htf_thisModulesTests


test_forkedThreadsRunFine = 
  replicateM 100 $ do
    var <- newIORef 0
    let increment = modifyIORef var (+1)
    S.fork $ do
      increment
      S.fork $ do
        increment
      S.fork $ do
        increment
      threadDelay $ 10^5
    threadDelay $ 20 * 10^3
    assertEqual 3 =<< readIORef var

test_killingAThreadKillsDeepSlaves = 
  replicateM 1000 $ do
    var <- newIORef 0
    let increment = modifyIORef var (+1)
    thread <- 
      S.fork $ do
        S.fork $ do
          S.fork $ do
            threadDelay $ 10^5
            increment
          S.fork $ do
            increment
          threadDelay $ 10^6
        threadDelay $ 10^6
    threadDelay $ 10^4
    killThread thread
    assertEqual 1 =<< readIORef var

test_dyingNormallyKillsSlaves = 
  replicateM 100 $ do
    var <- newIORef 0
    let increment = modifyIORef var (+1)
    S.fork $ do
      S.fork $ do
        threadDelay $ 10^4
        increment
      S.fork $ do
        threadDelay $ 10^4
        increment
    threadDelay $ 10^5
    assertEqual 0 =<< readIORef var

test_finalizationOrder = 
  replicateM 1000 $ do
    var <- newIORef 0
    S.forkFinally (modifyIORef var (*2)) $ do
      S.forkFinally (modifyIORef var (+1)) $ return ()
      S.forkFinally (modifyIORef var (+1)) $ return ()
    threadDelay $ 10^4
    assertEqual 4 =<< readIORef var

test_exceptionsDontGetLost = 
  replicateM 100 $ do
    assertThrowsSomeIO $ do
      S.fork $ do
        S.fork $ do
          error "!"
        threadDelay $ 10^4
      threadDelay $ 10^4


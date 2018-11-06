module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck
import qualified SlaveThread as S
import qualified Control.Concurrent.SSem as SSem


main =
  defaultMain $
  testGroup "All" $ [
    testCase "Failing in finalizer doesn't break everything" $ do
      finalizer1CalledVar <- newTVarIO False
      finalizer2CalledVar <- newTVarIO False
      result <- let
        finalizer1 =
          atomically $ writeTVar finalizer1CalledVar True
        finalizer2 =
          do
            atomically $ writeTVar finalizer2CalledVar True
            throwIO (userError "finalizer2 failed")
        in do
          try @SomeException $ do
            S.forkFinally finalizer1 $ do
              S.forkFinally finalizer2 $ threadDelay 100
      threadDelay (10^4)
      assertEqual "" "Left user error (finalizer2 failed)" (show result)
      finalizer2Called <- atomically (readTVar finalizer2CalledVar)
      finalizer1Called <- atomically (readTVar finalizer1CalledVar)
      assertEqual "" True finalizer2Called
      assertEqual "" True finalizer1Called
    ,
    testCase "Forked threads run fine" $ do
      replicateM_ 100000 $ do
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
        assertEqual "" 3 =<< readMVar var
    ,
    testCase "Killing a thread kills deep slaves" $ do
      replicateM_ 100000 $ do
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
        assertEqual "" 0 =<< readMVar var
    ,
    testCase "Dying normally kills slaves" $ do
      replicateM_ 100000 $ do
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
        assertEqual "" 0 =<< readIORef var
    ,
    testCase "Finalization is in order" $ do
      replicateM_ 100000 $ do
        var <- newMVar []
        semaphore <- SSem.new 0
        S.forkFinally (uninterruptibleMask_ (modifyMVar_ var (return . (1:)) >> SSem.signal semaphore)) $ do
          semaphore' <- SSem.new 0
          S.forkFinally (uninterruptibleMask_ (modifyMVar_ var (return . (2:)) >> SSem.signal semaphore')) $ do
            S.forkFinally (uninterruptibleMask_ (modifyMVar_ var (return . (3:)))) $ return ()
            S.forkFinally (uninterruptibleMask_ (modifyMVar_ var (return . (3:)))) $ return ()
          SSem.wait semaphore'
        SSem.wait semaphore
        assertEqual "" [1,2,3,3] =<< readMVar var
    ,
    testCase "Exceptions don't get lost" $ do
      replicateM_ 100000 $ do
        result <- try @SomeException $ do
          S.fork $ do
            S.fork $ do
              error "!"
            threadDelay $ 10^6
          threadDelay $ 10^6
        assertBool "" (isLeft result)
  ]

forkWait :: IO a -> IO (IO ())
forkWait io =
  do
    v <- newEmptyMVar
    S.fork $ do
      r <- try io
      putMVar v ()
      either (throwIO @SomeException) return r
    return $ takeMVar v

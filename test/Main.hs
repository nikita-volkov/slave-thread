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
        in try @SomeException $ do
          S.forkFinally finalizer1 $ do
            S.forkFinally finalizer2 $ threadDelay 100
          threadDelay (10^4)
      finalizer2Called <- atomically (readTVar finalizer2CalledVar)
      finalizer1Called <- atomically (readTVar finalizer1CalledVar)
      assertEqual "finalizer2 not called" True finalizer2Called
      assertEqual "finalizer1 not called" True finalizer1Called
      assertEqual "Invalid result" "Left user error (finalizer2 failed)" (show result)
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
    ,
    testCase "Slaves are finalized before master" $ do
      replicateM_ 100000 $ do
        ready <- newEmptyMVar
        var <- newEmptyTMVarIO
        thread <-
          S.forkFinally (atomically (tryPutTMVar var 1)) $ do
            S.forkFinally (atomically (tryPutTMVar var 0)) $
              threadDelay $ 10^6
            putMVar ready ()
            threadDelay $ 10^6
        takeMVar ready
        killThread thread
        assertEqual "First finalizer is not slave" 0 =<< atomically (readTMVar var)
    ,
    testCase "Slave thread finalizer is not interrupted by its own death (#11)" $ do
      -- Set up the ref that should be written to by thread 2's finalizer,
      -- otherwise there's a bug.
      ref <- newIORef True

      -- Let the main thread know when it should check the above IORef.
      done <- newEmptyMVar

      -- The gist of the test below: assert that, when a thread fails (here, the
      -- inner thread), its finalizer is not interrupted by a ThreadKilled
      -- thrown by the parent, which was originally triggered by its own death.

      S.forkFinally (putMVar done ()) $ do

        -- Let thread 2 know when it should die, with thread 1's exception
        -- handler in place.
        ready <- newEmptyMVar

        S.forkFinally
          (catch @SomeException (threadDelay (10^5)) (\_ -> writeIORef ref False)) $ do

          -- Wait until thread 1 is ready for us to die.
          takeMVar ready

          -- Die.
          throwIO (userError "")

        catch @SomeException
          ( -- Tell thread 2 we're ready for it to die
            putMVar ready () >>

            -- Sleep until thread 2 kills us.
            threadDelay (10^5*2)
          )
          -- Ignore thread 2's exception, so we don't propagate it up to the
          -- main thread.
          (\ _ -> return ())

      takeMVar done
      assertBool "Slave thread finalizer interrupted" =<< readIORef ref
    ,
    testCase "Master kills all slaves, even if it is thrown an exception during (#13)" $ do
      survived <- newEmptyTMVarIO
      ready <- newEmptyMVar
      done <- newEmptyMVar
      thread <-
        S.fork $ do
          S.forkFinally (atomically (tryPutTMVar survived True)) $ do
            uninterruptibleMask_ (putMVar ready () >> threadDelay (10^6))
            atomically (putTMVar survived False)
          takeMVar ready
          putMVar done ()
      takeMVar done
      threadDelay $ 10^5 -- be reasonably sure it's trying to kill its child
      killThread thread
      assertBool "Slave thread not killed by master" =<< atomically (takeTMVar survived)
  ]

forkWait :: IO a -> IO (IO ())
forkWait io =
  do
    v <- newEmptyMVar
    S.fork $ do
      r <- try @SomeException io
      putMVar v ()
      either throwIO return r
    return $ takeMVar v

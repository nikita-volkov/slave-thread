module SlaveThread.Util.IO where

import SlaveThread.Prelude


-- |
-- A more efficient version of 'forkIO',
-- which does not install a default exception handler on the forked thread.
{-# INLINE forkIOWithoutHandler #-}
forkIOWithoutHandler :: IO () -> IO ThreadId
forkIOWithoutHandler action = 
  IO $ \s -> case (fork# action s) of (# s', tid #) -> (# s', ThreadId tid #)

-- |
-- A more efficient version of 'forkIOWithUnmask',
-- which does not install a default exception handler on the forked thread.
{-# INLINE forkIOWithUnmaskWithoutHandler #-}
forkIOWithUnmaskWithoutHandler :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
forkIOWithUnmaskWithoutHandler action =
  forkIOWithoutHandler (action unsafeUnmask)

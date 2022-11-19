module SlaveThread.Util.LowLevelForking where

import SlaveThread.Prelude


-- |
-- A more efficient version of 'forkIO',
-- which does not install a default exception handler on the forked thread.
{-# INLINE forkIOWithoutHandler #-}
forkIOWithoutHandler :: IO () -> IO ThreadId
forkIOWithoutHandler (IO action) =
  IO $ \s -> case (fork# action s) of (# s', tid #) -> (# s', ThreadId tid #)

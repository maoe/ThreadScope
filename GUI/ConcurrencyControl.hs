
module GUI.ConcurrencyControl (
    ConcurrencyControl,
    start,
    fullSpeed,
  ) where

import qualified GI.GLib as GLib
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception  as Exception
import Control.Concurrent.MVar
import Data.Word (Word32)

newtype ConcurrencyControl = ConcurrencyControl (MVar (Int, HandlerId))
type HandlerId = Word32

-- | Setup cooperative thread scheduling with Gtk+.
--
start :: IO ConcurrencyControl
start = do
  handlerId <- normalScheduling
  return . ConcurrencyControl =<< newMVar (0, handlerId)

-- | Run an expensive action that needs to use all the available CPU power.
--
-- The normal cooperative GUI thread scheduling does not work so well in this
-- case so we use an alternative technique. We can't use this one all the time
-- however or we'd hog the CPU even when idle.
--
fullSpeed :: ConcurrencyControl -> IO a -> IO a
fullSpeed (ConcurrencyControl handlerRef) =
    Exception.bracket_ begin end
  where
    -- remove the normal scheduling handler and put in the full speed one
    begin = do
      (count, handlerId) <- takeMVar handlerRef
      if count == 0
        -- nobody else is running fullSpeed
        then do GLib.sourceRemove handlerId
                handlerId' <- fullSpeedScheduling
                putMVar handlerRef (1, handlerId')
        -- we're already running fullSpeed, just inc the count
        else do putMVar handlerRef (count+1, handlerId)

    -- reinstate the normal scheduling
    end = do
      (count, handlerId) <- takeMVar handlerRef
      if count == 1
        -- just us running fullSpeed so we clean up
        then do GLib.sourceRemove handlerId
                handlerId' <- normalScheduling
                putMVar handlerRef (0, handlerId')
        -- someone else running fullSpeed, they're responsible for stopping
        else do putMVar handlerRef (count-1, handlerId)

normalScheduling :: IO HandlerId
normalScheduling =
  GLib.timeoutAdd
    GLib.PRIORITY_DEFAULT_IDLE
    50 --50ms, ie 20 times a second.
    (Concurrent.yield >> return True)

fullSpeedScheduling :: IO HandlerId
fullSpeedScheduling =
  GLib.idleAdd
    GLib.PRIORITY_DEFAULT_IDLE
    (Concurrent.yield >> return True)

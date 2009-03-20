-------------------------------------------------------------------------------
--- $Id: ReadEvents.hs#7 2009/03/10 16:01:35 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ghc-profiling/Events/ReadEvents.hs $
-------------------------------------------------------------------------------

module ReadEvents
where

import Data.Array
import qualified Data.Function
import Data.IORef
import Data.List
import Text.Printf

import EventlogViewerCommon

import Graphics.UI.Gtk

import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

-------------------------------------------------------------------------------

rawEventsToHECs :: [GHCEvents.Event] -> HECs
rawEventsToHECs eventList
  = [filterHEC eventList hec | hec <- capabilities]
    where
    capabilities = ennumerateCapabilities eventList
    
-------------------------------------------------------------------------------

filterHEC events hec
  = (hec, listArray (0, nrEvents-1) eventsForThisHEC)
    where
    eventsForThisHEC = filter (eventFromHEC hec) events
    nrEvents = length eventsForThisHEC
 
-------------------------------------------------------------------------------

eventFromHEC :: Int -> GHCEvents.Event -> Bool
eventFromHEC hec event 
  = cap (spec event) == hec

-------------------------------------------------------------------------------

registerEventsFromFile :: String -> IORef (Maybe [Int]) -> MaybeHECsIORef ->
                          IORef Double -> IORef Integer ->
                          Window -> Label -> Statusbar -> ContextId -> IO ()
registerEventsFromFile filename capabilitiesIORef eventArrayIORef scale
                       lastTxIORef window profileNameLabel summarybar
                       summary_ctx
  = do fmt <- buildFormat filename 
       let pes = events (fmtData fmt)
           sorted = sortBy (Data.Function.on compare ts) (reverse pes)
           hecs = rawEventsToHECs sorted
           lastTx = event2ms (last sorted) -- Last event time in ms
           capabilities = ennumerateCapabilities pes
       -- Update the IORefs used for drawing callbacks
       writeIORef capabilitiesIORef (Just capabilities)
       writeIORef eventArrayIORef (Just hecs)
       writeIORef lastTxIORef lastTx
       writeIORef scale defaultScaleValue
       let duration = lastTx 
           nrEvents = length pes

       -- Adjust height to fit capabilities
       (width, _) <- widgetGetSize window
       widgetSetSizeRequest window width ((length capabilities)*gapcap+oycap+120)

       let w = if width == 1 then
                 1450
               else
                 width
       -- Make the defualt view fit the whole trace.
       writeIORef scale (0.9* (fromIntegral w) / (fromIntegral duration))

       -- Set the status bar
       statusbarPush summarybar summary_ctx (show nrEvents ++ " events. Duration " ++ (printf "%.3f" (((fromIntegral duration)::Double) * 1.0e-6)) ++ " seconds.")   


       ------------------------------------------------------------------------
       --- Set the label for the name of the event log
       profileNameLabel `labelSetText` filename

       
-------------------------------------------------------------------------------


module State ( ViewerState(..) ) where

import EventlogViewerCommon

import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

import Graphics.UI.Gtk

import Data.IORef
import Data.Array

-----------------------------------------------------------------------------

data ViewerState = ViewerState {
  filenameIORef    :: IORef (Maybe FilePath),
  debug            :: Bool,

  -- The loaded profile
  capabilitiesIORef :: IORef (Maybe [Int]),
  hecsIORef         :: MaybeHECsIORef,
  lastTxIORef       :: IORef Timestamp,
  eventArrayIORef   :: IORef (Array Int GHCEvents.CapEvent),

  -- current scale factor
  scaleIORef        :: IORef Double,

  -- WIDGETS
  
  -- main window
  mainWindow         :: Window,
  summaryBar         :: Statusbar,
  statusBar          :: Statusbar,

  -- menu items
  bwToggle           :: CheckMenuItem,
  fullDetailToggle   :: CheckMenuItem,
  openMenuItem       :: MenuItem,
  saveMenuItem       :: MenuItem,
  saveAsMenuItem     :: MenuItem,
  reloadMenuItem     :: MenuItem,
  quitMenuItem       :: MenuItem,
  aboutMenuItem      :: MenuItem,

  -- CPUs view
  profileDrawingArea :: DrawingArea,
  profileHScrollbar  :: HScrollbar,
  profileAdj         :: Adjustment,
  zoomInButton       :: ToolButton,
  zoomOutButton      :: ToolButton,
  zoomFitButton      :: ToolButton,
  showLabelsToggle   :: ToggleToolButton,
  capDrawingArea     :: DrawingArea,
  keyDrawingArea     :: DrawingArea,

  -- Events view
  eventsVScrollbar   :: VScrollbar,
  eventsDrawingArea  :: DrawingArea

  }

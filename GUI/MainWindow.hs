{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE ScopedTypeVariables #-}
module GUI.MainWindow (
    MainWindow,
    mainWindowNew,
    MainWindowActions(..),

    setFileLoaded,
    setStatusMessage,
    sidebarSetVisibility,
    eventsSetVisibility,

  ) where
import Data.Foldable (traverse_)
import Data.Word (Word32)

import GI.Gtk as Gtk
import qualified Data.Text as T

import GUI.DataFiles (loadLogo)

-------------------------------------------------------------------------------

data MainWindow = MainWindow {
       mainWindow         :: Window,

       sidebarBox,
       eventsBox          :: Widget,

       statusBar          :: Statusbar,
       statusBarCxt       :: ContextId
     }

type ContextId = Word32

-- instance GObject MainWindow
-- instance O.HasParentTypes MainWindow
-- type instance O.ParentTypes MainWindow =
--   '[Object, Widget, Container, Bin, Window]

data MainWindowActions = MainWindowActions {

       -- Menu actions
       mainWinOpen          :: IO (),
       mainWinExport        :: IO (),
       mainWinQuit          :: IO (),
       mainWinViewSidebar   :: Bool -> IO (),
       mainWinViewEvents    :: Bool -> IO (),
       mainWinViewBW        :: Bool -> IO (),
       mainWinViewReload    :: IO (),
       mainWinWebsite       :: IO (),
       mainWinTutorial      :: IO (),
       mainWinAbout         :: IO (),

       -- Toolbar actions
       mainWinJumpStart     :: IO (),
       mainWinJumpEnd       :: IO (),
       mainWinJumpCursor    :: IO (),
       mainWinJumpZoomIn    :: IO (),
       mainWinJumpZoomOut   :: IO (),
       mainWinJumpZoomFit   :: IO (),
       mainWinScrollLeft    :: IO (),
       mainWinScrollRight   :: IO (),
       mainWinDisplayLabels :: Bool -> IO ()
     }

-------------------------------------------------------------------------------

setFileLoaded :: MainWindow -> Maybe FilePath -> IO ()
setFileLoaded mainWin Nothing =
  set (mainWindow mainWin) [
      windowTitle := "ThreadScope"
    ]
setFileLoaded mainWin (Just file) =
  set (mainWindow mainWin) [
      windowTitle := T.pack $ file ++ " - ThreadScope"
    ]

setStatusMessage :: MainWindow -> T.Text -> IO ()
setStatusMessage mainWin msg = do
  statusbarPop  (statusBar mainWin) (statusBarCxt mainWin)
  statusbarPush (statusBar mainWin) (statusBarCxt mainWin) (" " <> msg)
  return ()

sidebarSetVisibility :: MainWindow -> Bool -> IO ()
sidebarSetVisibility mainWin visible =
  set (sidebarBox mainWin) [ widgetVisible := visible ]

eventsSetVisibility :: MainWindow -> Bool -> IO ()
eventsSetVisibility mainWin visible =
  set (eventsBox mainWin) [ widgetVisible := visible ]

-------------------------------------------------------------------------------

mainWindowNew :: Builder -> MainWindowActions -> IO MainWindow
mainWindowNew builder actions = do

  let getWidget :: GObject a => (ManagedPtr a -> a) -> T.Text -> IO a
      getWidget ctor name = builderGetObject builder name
        >>= maybe
          (fail ("object not found: " ++ T.unpack name))
          (unsafeCastTo ctor)


  mainWindow         <- getWidget Window "main_window"
  statusBar :: Statusbar <- getWidget Statusbar "statusbar"

  sidebarBox         <- getWidget Widget "sidebar"
  eventsBox          <- getWidget Widget "eventsbox"

  bwToggle           <- getWidget CheckMenuItem "black_and_white"
  labModeToggle      <- getWidget CheckMenuItem "view_labels_mode"
  sidebarToggle      <- getWidget CheckMenuItem "view_sidebar"
  eventsToggle       <- getWidget CheckMenuItem "view_events"
  openMenuItem       <- getWidget MenuItem "openMenuItem"
  exportMenuItem     <- getWidget MenuItem "exportMenuItem"
  reloadMenuItem     <- getWidget MenuItem "view_reload"
  quitMenuItem       <- getWidget MenuItem "quitMenuItem"
  websiteMenuItem    <- getWidget MenuItem "websiteMenuItem"
  tutorialMenuItem   <- getWidget MenuItem "tutorialMenuItem"
  aboutMenuItem      <- getWidget MenuItem "aboutMenuItem"

  firstMenuItem      <- getWidget MenuItem "move_first"
  centreMenuItem     <- getWidget MenuItem "move_centre"
  lastMenuItem       <- getWidget MenuItem "move_last"

  zoomInMenuItem     <- getWidget MenuItem "move_zoomin"
  zoomOutMenuItem    <- getWidget MenuItem "move_zoomout"
  zoomFitMenuItem    <- getWidget MenuItem "move_zoomfit"

  openButton         <- getWidget ToolButton "cpus_open"

  firstButton        <- getWidget ToolButton "cpus_first"
  centreButton       <- getWidget ToolButton "cpus_centre"
  lastButton         <- getWidget ToolButton "cpus_last"

  zoomInButton       <- getWidget ToolButton "cpus_zoomin"
  zoomOutButton      <- getWidget ToolButton "cpus_zoomout"
  zoomFitButton      <- getWidget ToolButton "cpus_zoomfit"

  ------------------------------------------------------------------------
  -- Show everything
  widgetShowAll mainWindow

  ------------------------------------------------------------------------

  $loadLogo >>= traverse_ (\logo -> set mainWindow [ #icon := logo ])

  ------------------------------------------------------------------------
  -- Status bar functionality

  statusBarCxt <- statusbarGetContextId statusBar "file"
  statusbarPush statusBar statusBarCxt "No eventlog loaded."

  ------------------------------------------------------------------------
  -- Bind all the events

  -- Menus
  on openMenuItem #activate $ mainWinOpen actions
  on exportMenuItem #activate $ mainWinExport actions

  on quitMenuItem #activate $ mainWinQuit actions
  on mainWindow #destroy $ mainWinQuit actions

  on sidebarToggle #toggled $ checkMenuItemGetActive sidebarToggle
                                       >>= mainWinViewSidebar   actions
  on eventsToggle #toggled $ checkMenuItemGetActive eventsToggle
                                       >>= mainWinViewEvents    actions
  on bwToggle #toggled $ checkMenuItemGetActive bwToggle
                                       >>= mainWinViewBW        actions
  on labModeToggle #toggled $ checkMenuItemGetActive labModeToggle
                                       >>= mainWinDisplayLabels actions
  on reloadMenuItem #activate $ mainWinViewReload actions

  on websiteMenuItem #activate $ mainWinWebsite actions
  on tutorialMenuItem #activate $ mainWinTutorial actions
  on aboutMenuItem #activate $ mainWinAbout actions

  on firstMenuItem #activate $ mainWinJumpStart  actions
  on centreMenuItem #activate $ mainWinJumpCursor actions
  on lastMenuItem #activate $ mainWinJumpEnd    actions

  on zoomInMenuItem #activate $ mainWinJumpZoomIn  actions
  on zoomOutMenuItem #activate $ mainWinJumpZoomOut actions
  on zoomFitMenuItem #activate $ mainWinJumpZoomFit actions

  -- Toolbar
  onToolButtonClicked openButton $ mainWinOpen       actions

  onToolButtonClicked firstButton  $ mainWinJumpStart  actions
  onToolButtonClicked centreButton $ mainWinJumpCursor actions
  onToolButtonClicked lastButton   $ mainWinJumpEnd    actions

  onToolButtonClicked zoomInButton  $ mainWinJumpZoomIn  actions
  onToolButtonClicked zoomOutButton $ mainWinJumpZoomOut actions
  onToolButtonClicked zoomFitButton $ mainWinJumpZoomFit actions

  return MainWindow {..}

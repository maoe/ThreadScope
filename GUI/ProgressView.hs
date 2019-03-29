{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module GUI.ProgressView (
    ProgressView,
    withProgress,
    setText,
    setTitle,
    setProgress,
    startPulse,
  ) where

import GI.Cairo hiding (new)
import GI.Gtk as Gtk hiding (new)
import GUI.GtkExtras

import Control.Exception
import Data.Text (Text)
import Data.Typeable
import qualified Control.Concurrent as Concurrent

data ProgressView = ProgressView {
    progressWindow :: Gtk.Window,
    progressLabel  :: Gtk.Label,
    progressBar    :: Gtk.ProgressBar
  }

-- | Perform a long-running operation and display a progress window. The
-- operation has access to the progress window and it is expected to update it
-- using 'setText' and 'setProgress'
--
-- The user may cancel the operation at any time.
--
withProgress :: IsWindow win => win -> (ProgressView -> IO a) -> IO (Maybe a)
withProgress parent action = do
  self <- Concurrent.myThreadId
  let cancel = throwTo self OperationInterrupted
  bracket (new parent cancel) close $ \progress ->
    fmap Just (action progress)
      `catch` \OperationInterrupted -> return Nothing

data OperationInterrupted = OperationInterrupted
  deriving (Typeable, Show)
instance Exception OperationInterrupted

setText :: ProgressView -> Text -> IO ()
setText view msg =
  set (progressBar view) [
    progressBarText := msg
  ]

setTitle :: ProgressView -> Text -> IO ()
setTitle view msg = do
  set (progressWindow view) [ windowTitle := msg ]
  set (progressLabel view)  [ labelLabel  := "<b>" <> msg <> "</b>" ]

startPulse :: ProgressView -> IO (IO ())
startPulse view = do
  let pulse = do
        progressBarPulse (progressBar view)
        Concurrent.threadDelay 200000
        pulse
  thread <- Concurrent.forkIO $
              pulse `catch` \OperationInterrupted -> return ()
  let stop = throwTo thread OperationInterrupted
  waitGUI
  return stop

setProgress :: ProgressView -> Int -> Int -> IO ()
setProgress view total current = do
  let frac = fromIntegral current / fromIntegral total
  set (progressBar view) [ progressBarFraction := frac ]
  waitGUI

close :: ProgressView -> IO ()
close view = widgetDestroy (progressWindow view)

new :: IsWindow win => win -> IO () -> IO ProgressView
new parent cancelAction = do
  win <- windowNew WindowTypeToplevel
  parentWin <- toWindow parent
  set win [
      #borderWidth := 10,
      #title := "",
      #transientFor := parentWin,
      #modal := True,
      #windowPosition := WindowPositionCenterOnParent,
      #defaultWidth := 400,
      #skipTaskbarHint := True
    ]

  progText <- labelNew Nothing
  set progText [
      miscXalign := 0,
      labelUseMarkup := True
    ]

  progress <- progressBarNew

  cancel <- buttonNewWithLabel "Cancel"
  on cancel #clicked $ widgetDestroy win >> cancelAction
  on win #destroy cancelAction
  on win #keyPressEvent $ \event -> do
    keyVal <- get event #keyval
    case keyVal of
      0xff1b -> cancelAction >> return True
      _      -> return False

  vbox <- vBoxNew False 20
  hbox <- hBoxNew False 0
  boxPackStart vbox progText True False 10
  boxPackStart vbox progress True True  5
  boxPackStart vbox hbox     False False 5
  boxPackEnd   hbox cancel   False False 0
  containerAdd win vbox

  widgetShowAll win

  return ProgressView {
    progressWindow = win,
    progressLabel  = progText,
    progressBar    = progress
  }

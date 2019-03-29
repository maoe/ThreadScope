{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
module GUI.GtkExtras
  ( waitGUI
  , getWidgetAs
  , launchProgramForURI
  ) where

import GI.Gtk
import GI.GLib

import Control.Concurrent.MVar
import Data.Text (Text)
import qualified Data.Text as T

#if mingw32_HOST_OS || mingw32_TARGET_OS
#include "windows_cconv.h"
#endif

waitGUI :: IO ()
waitGUI = do
  resultVar <- newEmptyMVar
  idleAdd PRIORITY_DEFAULT_IDLE (putMVar resultVar () >> return False)
  takeMVar resultVar

getWidgetAs
  :: GObject o
  => Builder
  -> Text
  -> (ManagedPtr o -> o)
  -> IO o
getWidgetAs builder name ctor = #getObject builder name >>= \case
  Just obj -> castTo ctor obj >>= \case
    Just widget -> return widget
    Nothing -> fail $ "Object named '" ++ T.unpack name ++ "' could not cast."
  Nothing -> fail $ "Object named '" ++ T.unpack name ++ "' could not be found."

-------------------------------------------------------------------------------

launchProgramForURI :: Text -> IO Bool
#if mingw32_HOST_OS || mingw32_TARGET_OS
launchProgramForURI uri = do
    withCString "open" $ \verbPtr ->
      withCString uri $ \filePtr ->
        c_ShellExecuteA
            nullPtr
            verbPtr
            filePtr
            nullPtr
            nullPtr
            1       -- SW_SHOWNORMAL
    return True

foreign import WINDOWS_CCONV unsafe "shlobj.h ShellExecuteA"
    c_ShellExecuteA :: Ptr ()  -- HWND hwnd
                    -> CString -- LPCTSTR lpOperation
                    -> CString -- LPCTSTR lpFile
                    -> CString -- LPCTSTR lpParameters
                    -> CString -- LPCTSTR lpDirectory
                    -> CInt    -- INT nShowCmd
                    -> IO CInt -- HINSTANCE return

#else
launchProgramForURI uri = do
  timestamp <- getCurrentEventTime
  showUriOnWindow (Nothing :: Maybe Window) uri timestamp
  return True
#endif

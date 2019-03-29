{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-------------------------------------------------------------------------------
-- | Module : GUI.App
--
-- Platform-specific application functionality
-------------------------------------------------------------------------------

module GUI.App (initApp) where

-- Mac OS X-specific GTK imports
#if defined(darwin_HOST_OS)
import Data.Foldable (for_)
import GUI.DataFiles (loadLogo)
import qualified GI.Gtk as Gtk
import qualified GI.GtkosxApplication as OSX
#endif

-------------------------------------------------------------------------------


-- | Initialize application
-- Perform platform-specific application initialization
initApp :: IO ()
initApp = do
#if defined(darwin_HOST_OS)
  app <- Gtk.new OSX.Application []
  menuBar <- Gtk.menuBarNew
  OSX.applicationSetMenuBar app menuBar
  logo <- $loadLogo
  for_ logo $ OSX.applicationSetDockIconPixbuf app
  OSX.applicationReady app
#endif
  return ()

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.StartupInfoView (
    StartupInfoView,
    startupInfoViewNew,
    startupInfoViewSetEvents,
  ) where

import GHC.RTS.Events

import GI.Gtk
import Data.GI.Gtk

import Data.Array
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Text as T

-------------------------------------------------------------------------------

data StartupInfoView = StartupInfoView
     { labelProgName      :: Label
     , storeProgArgs      :: SeqStore String
     , storeProgEnv       :: SeqStore (String, String)
     , labelProgStartTime :: Label
     , labelProgRtsId     :: Label
     }

data StartupInfoState
   = StartupInfoEmpty
   | StartupInfoLoaded
     { progName      :: Maybe String
     , progArgs      :: Maybe [String]
     , progEnv       :: Maybe [(String, String)]
     , progStartTime :: Maybe UTCTime
     , progRtsId     :: Maybe String
     }

-------------------------------------------------------------------------------

startupInfoViewNew :: Builder -> IO StartupInfoView
startupInfoViewNew builder = do

    let getWidget ctor name = builderGetObject builder name
          >>= maybe
            (fail ("object not found: " ++ T.unpack name))
            (unsafeCastTo ctor)

    labelProgName      <- getWidget Label    "labelProgName"
    treeviewProgArgs   <- getWidget TreeView "treeviewProgArguments"
    treeviewProgEnv    <- getWidget TreeView "treeviewProgEnvironment"
    labelProgStartTime <- getWidget Label    "labelProgStartTime"
    labelProgRtsId     <- getWidget Label    "labelProgRtsIdentifier"

    storeProgArgs    <- seqStoreNew []
    columnArgs       <- treeViewColumnNew
    cellArgs         <- cellRendererTextNew

    treeViewColumnPackStart columnArgs cellArgs True
    treeViewAppendColumn treeviewProgArgs columnArgs

    treeViewSetModel treeviewProgArgs (Just storeProgArgs)

    set cellArgs [ #editable := True ]
    cellLayoutSetAttributes columnArgs cellArgs storeProgArgs $ \arg ->
      [ #text := T.pack arg ]

    storeProgEnv     <- seqStoreNew []
    columnVar        <- treeViewColumnNew
    cellVar          <- cellRendererTextNew
    columnValue      <- treeViewColumnNew
    cellValue        <- cellRendererTextNew

    treeViewColumnPackStart columnVar   cellVar   False
    treeViewColumnPackStart columnValue cellValue True
    treeViewAppendColumn treeviewProgEnv columnVar
    treeViewAppendColumn treeviewProgEnv columnValue

    treeViewSetModel treeviewProgEnv (Just storeProgEnv)

    cellLayoutSetAttributes columnVar cellVar storeProgEnv $ \(var,_) ->
      [ #text := T.pack var ]

    set cellValue [ #editable := True ]
    cellLayoutSetAttributes columnValue cellValue storeProgEnv $ \(_,value) ->
      [ #text := T.pack value ]

    let startupInfoView = StartupInfoView{..}

    return startupInfoView

-------------------------------------------------------------------------------

startupInfoViewSetEvents :: StartupInfoView -> Maybe (Array Int Event) -> IO ()
startupInfoViewSetEvents view mevents =
    updateStartupInfo view (maybe StartupInfoEmpty processEvents mevents)

--TODO: none of this handles the possibility of an eventlog containing multiple
-- OS processes. Note that the capset arg is ignored in the events below.

processEvents :: Array Int Event -> StartupInfoState
processEvents = foldl' accum (StartupInfoLoaded Nothing Nothing Nothing Nothing Nothing)
              . take 1000
              . elems
  where
    accum info (Event _ (ProgramArgs _ (name:args)) _) =
      info {
        progName = Just name,
        progArgs = Just args
      }

    accum info (Event _ (ProgramEnv _ env) _) =
      info { progEnv = Just (sort (parseEnv env)) }

    accum info (Event _ (RtsIdentifier _ rtsid) _) =
      info { progRtsId = Just rtsid }

    accum info (Event timestamp (WallClockTime _ sec nsec) _) =
          -- WallClockTime records the wall clock time of *this* event
          -- which occurs some time after startup, so we can just subtract
          -- the timestamp since that is the relative time since startup.
      let wallTimePosix :: NominalDiffTime
          wallTimePosix = fromIntegral sec
                        + (fromIntegral nsec / nanoseconds)
                        - (fromIntegral timestamp / nanoseconds)
          nanoseconds   = 1000000000
          wallTimeUTC   = posixSecondsToUTCTime wallTimePosix
      in  info { progStartTime = Just wallTimeUTC }

    accum info _ = info

    -- convert ["foo=bar", ...] to [("foo", "bar"), ...]
    parseEnv env = [ (var, value) | (var, '=':value) <- map (span (/='=')) env ]

updateStartupInfo :: StartupInfoView -> StartupInfoState -> IO ()
updateStartupInfo StartupInfoView{..} StartupInfoLoaded{..} = do
    set labelProgName      [ #label := maybe "(unknown)" T.pack progName ]
    set labelProgStartTime [ #label := maybe "(unknown)" (T.pack . show) progStartTime ]
    set labelProgRtsId     [ #label := maybe "(unknown)" T.pack progRtsId ]
    seqStoreClear storeProgArgs
    mapM_ (seqStoreAppend storeProgArgs) (fromMaybe [] progArgs)
    seqStoreClear storeProgEnv
    mapM_ (seqStoreAppend storeProgEnv) (fromMaybe [] progEnv)

updateStartupInfo StartupInfoView{..} StartupInfoEmpty = do
    set labelProgName      [ #label := "" ]
    set labelProgStartTime [ #label := "" ]
    set labelProgRtsId     [ #label := "" ]
    seqStoreClear storeProgArgs
    seqStoreClear storeProgEnv

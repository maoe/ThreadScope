{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.TraceView (
    TraceView,
    traceViewNew,
    TraceViewActions(..),
    traceViewSetHECs,
    traceViewGetTraces,
  ) where
import Control.Monad (void)
import Data.Foldable (for_)

import Events.HECs
import GUI.Types

import GI.Gtk
import Data.GI.Gtk
import Data.Tree
import qualified Data.Text as T

-- | Abstract trace view object.
--
data TraceView = TraceView {
       tracesStore :: ForestStore (Trace, Visibility)
     }

data Visibility = Visible | Hidden | MixedVisibility
  deriving Eq

-- | The actions to take in response to TraceView events.
--
data TraceViewActions = TraceViewActions {
       traceViewTracesChanged :: [Trace] -> IO ()
     }

traceViewNew :: Builder -> TraceViewActions -> IO TraceView
traceViewNew builder actions = do
    let getWidget ctor name = builderGetObject builder name
          >>= maybe
            (fail ("object not found: " ++ T.unpack name))
            (unsafeCastTo ctor)
    tracesTreeView <- getWidget TreeView "traces_tree"

    tracesStore <- forestStoreNew []
    traceColumn <- treeViewColumnNew
    textcell    <- cellRendererTextNew
    togglecell  <- cellRendererToggleNew

    let traceview = TraceView {..}

    treeViewColumnPackStart traceColumn textcell   True
    treeViewColumnPackStart traceColumn togglecell False
    treeViewAppendColumn tracesTreeView traceColumn

    treeViewSetModel tracesTreeView (Just tracesStore)

    cellLayoutSetAttributes traceColumn textcell tracesStore $ \(tr, _) ->
      [ #text := renderTrace tr ]

    cellLayoutSetAttributes traceColumn togglecell tracesStore $ \(_, vis) ->
      [ #active       := vis == Visible
      , #inconsistent := vis == MixedVisibility ]

    on togglecell #toggled $ \str ->  do
      path <- treePathNewFromString str
      Node (trace, visibility) subtrees <- forestStoreGetTree tracesStore path
      let visibility' = invertVisibility visibility
      forestStoreSetValue tracesStore path (trace, visibility')
      updateChildren tracesStore path subtrees visibility'
      updateParents tracesStore path

      traceViewTracesChanged actions =<< traceViewGetTraces traceview

    return traceview

  where
    renderTrace (TraceHEC           hec) = T.pack $ "HEC " ++ show hec
    renderTrace (TraceInstantHEC    hec) = T.pack $ "HEC " ++ show hec
    renderTrace (TraceCreationHEC   hec) = T.pack $ "HEC " ++ show hec
    renderTrace (TraceConversionHEC hec) = T.pack $ "HEC " ++ show hec
    renderTrace (TracePoolHEC       hec) = T.pack $ "HEC " ++ show hec
    renderTrace (TraceHistogram)         = "Spark Histogram"
    renderTrace (TraceGroup       label) = T.pack label
    renderTrace (TraceActivity)          = "Activity Profile"

    updateChildren
      :: ForestStore (Trace, Visibility)
      -> TreePath
      -> Forest (Trace, a)
      -> Visibility
      -> IO ()
    updateChildren tracesStore path subtrees visibility' = do
      for_ (zip subtrees [0..]) $ \(Node (trace, _) subtrees', n) -> do
        treePathAppendIndex path n
        forestStoreSetValue tracesStore path (trace, visibility')
        updateChildren tracesStore path subtrees' visibility'

    updateParents :: ForestStore (Trace, Visibility) -> TreePath -> IO ()
    updateParents tracesStore path = do
      hasParent <- treePathUp path
      if hasParent
        then do
          Node (trace, _) subtrees <- forestStoreGetTree tracesStore path
          let visibility = accumVisibility  [ vis | subtree  <- subtrees
                                                  , (_, vis) <- flatten subtree ]
          forestStoreSetValue tracesStore path (trace, visibility)
          updateParents tracesStore path
        else
          return ()

    invertVisibility Hidden = Visible
    invertVisibility _      = Hidden

    accumVisibility = foldr1 (\a b -> if a == b then a else MixedVisibility)

-- Find the HEC traces in the forestStore and replace them
traceViewSetHECs :: TraceView -> HECs -> IO ()
traceViewSetHECs TraceView{tracesStore} hecs = do
    forestStoreClear tracesStore
    path <- treePathNew -- Set to root initially
    -- for testing only (e.g., to compare with histogram of data from interval
    -- or to compare visually with other traces):
    -- forestStoreInsert tracesStore [] 0 (TraceHistogram, Visible)
    go path
    forestStoreInsert tracesStore path 0 (TraceActivity, Visible)
  where
    newT = Node { rootLabel = (TraceGroup "HEC Traces", Visible),
                  subForest = [ Node { rootLabel = (TraceHEC k, Visible),
                                       subForest = [] }
                              | k <- [ 0 .. hecCount hecs - 1 ] ] }
    newI = Node { rootLabel = (TraceGroup "Instant Events", Hidden),
                  subForest = [ Node { rootLabel = (TraceInstantHEC k, Hidden),
                                       subForest = [] }
                              | k <- [ 0 .. hecCount hecs - 1 ] ] }
    nCre = Node { rootLabel = (TraceGroup "Spark Creation", Hidden),
                  subForest = [ Node { rootLabel = (TraceCreationHEC k, Hidden),
                                       subForest = [] }
                              | k <- [ 0 .. hecCount hecs - 1 ] ] }
    nCon = Node { rootLabel = (TraceGroup "Spark Conversion", Hidden),
                  subForest = [ Node { rootLabel = (TraceConversionHEC k, Hidden),
                                       subForest = [] }
                              | k <- [ 0 .. hecCount hecs - 1 ] ] }
    nPoo = Node { rootLabel = (TraceGroup "Spark Pool", Hidden),
                  subForest = [ Node { rootLabel = (TracePoolHEC k, Hidden),
                                       subForest = [] }
                              | k <- [ 0 .. hecCount hecs - 1 ] ] }
    -- FIXME: correct the TreePath handling
    go path = do
      m <- forestStoreLookup tracesStore path
      case m of
        Nothing -> do
          forestStoreInsertTree tracesStore path 0 nPoo
          forestStoreInsertTree tracesStore path 0 nCon
          forestStoreInsertTree tracesStore path 0 nCre
          forestStoreInsertTree tracesStore path 0 newI
          forestStoreInsertTree tracesStore path 0 newT
        Just t  -> do
          case t of
             Node { rootLabel = (TraceGroup "HEC Traces", _) } -> do
               forestStoreRemove tracesStore path
               forestStoreInsertTree tracesStore path 0 newT
             Node { rootLabel = (TraceGroup "HEC Instant Events", _) } -> do
               forestStoreRemove tracesStore path
               forestStoreInsertTree tracesStore path 0 newI
             Node { rootLabel = (TraceGroup "Spark Creation", _) } -> do
               forestStoreRemove tracesStore path
               forestStoreInsertTree tracesStore path 0 nCre
             Node { rootLabel = (TraceGroup "Spark Conversion", _) } -> do
               forestStoreRemove tracesStore path
               forestStoreInsertTree tracesStore path 0 nCon
             Node { rootLabel = (TraceGroup "Spark Pool", _) } -> do
               forestStoreRemove tracesStore path
               forestStoreInsertTree tracesStore path 0 nPoo
             Node { rootLabel = (TraceActivity, _) } ->
               void $ forestStoreRemove tracesStore path
             _ -> return ()
          treePathNext path
          go path

traceViewGetTraces :: TraceView -> IO [Trace]
traceViewGetTraces TraceView{tracesStore} = do
  f <- getTracesStoreContents tracesStore
  return [ t | (t, Visible) <- concatMap flatten f, notGroup t ]
 where
  notGroup (TraceGroup _) = False
  notGroup _              = True

getTracesStoreContents :: ForestStore a -> IO (Forest a)
getTracesStoreContents tracesStore = treePathNewFirst >>= go
  where
  go path = do
    m <- forestStoreLookup tracesStore path
    case m of
      Nothing -> return []
      Just t  -> do
        treePathNext path
        ts <- go path
        return (t:ts)

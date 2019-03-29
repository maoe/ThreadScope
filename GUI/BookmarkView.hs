{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.BookmarkView (
    BookmarkView,
    bookmarkViewNew,
    BookmarkViewActions(..),

    bookmarkViewGet,
    bookmarkViewAdd,
    bookmarkViewRemove,
    bookmarkViewClear,
    bookmarkViewSetLabel,
  ) where

import GHC.RTS.Events (Timestamp)

import Control.Monad (join, when)
import Data.Foldable (for_)
import Data.GI.Gtk
import Data.Int (Int32)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Numeric
import qualified Data.Text as T

---------------------------------------------------------------------------

-- | Abstract bookmark view object.
--
data BookmarkView = BookmarkView {
       bookmarkStore :: SeqStore (Timestamp, Text)
     }

-- | The actions to take in response to TraceView events.
--
data BookmarkViewActions = BookmarkViewActions {
       bookmarkViewAddBookmark    :: IO (),
       bookmarkViewRemoveBookmark :: Int32 -> IO (),
       bookmarkViewGotoBookmark   :: Timestamp -> IO (),
       bookmarkViewEditLabel
          :: Text -- ^ The path identifying the edited cell
          -> Text -- ^ The new text
          -> IO ()
     }

---------------------------------------------------------------------------

bookmarkViewAdd :: BookmarkView -> Timestamp -> Text -> IO ()
bookmarkViewAdd BookmarkView{bookmarkStore} ts label = do
  seqStoreAppend bookmarkStore (ts, label)
  return ()

bookmarkViewRemove :: BookmarkView -> Int32 -> IO ()
bookmarkViewRemove BookmarkView{bookmarkStore} n = do
  seqStoreRemove bookmarkStore n
  return ()

bookmarkViewClear :: BookmarkView -> IO ()
bookmarkViewClear BookmarkView{bookmarkStore} =
  seqStoreClear bookmarkStore

bookmarkViewGet :: BookmarkView -> IO [(Timestamp, Text)]
bookmarkViewGet BookmarkView{bookmarkStore} =
  seqStoreToList bookmarkStore

bookmarkViewSetLabel :: BookmarkView -> Int32 -> Text -> IO ()
bookmarkViewSetLabel BookmarkView{bookmarkStore} n label = do
  (ts,_) <- seqStoreGetValue bookmarkStore n
  seqStoreSetValue bookmarkStore n (ts, label)

---------------------------------------------------------------------------

bookmarkViewNew :: Builder -> BookmarkViewActions -> IO BookmarkView
bookmarkViewNew builder BookmarkViewActions{..} = do

    let getWidget ctor name = builderGetObject builder name
          >>= maybe
            (fail ("object not found: " ++ T.unpack name))
            (unsafeCastTo ctor)

    ---------------------------------------------------------------------------

    bookmarkTreeView <- getWidget TreeView "bookmark_list"
    bookmarkStore    <- seqStoreNew []
    columnTs         <- treeViewColumnNew
    cellTs           <- cellRendererTextNew
    columnLabel      <- treeViewColumnNew
    cellLabel        <- cellRendererTextNew
    selection        <- treeViewGetSelection bookmarkTreeView

    treeViewColumnSetTitle columnTs    "Time"
    treeViewColumnSetTitle columnLabel "Label"
    treeViewColumnPackStart columnTs    cellTs    False
    treeViewColumnPackStart columnLabel cellLabel True
    treeViewAppendColumn bookmarkTreeView columnTs
    treeViewAppendColumn bookmarkTreeView columnLabel

    treeViewSetModel bookmarkTreeView (Just bookmarkStore)

    cellLayoutSetAttributes columnTs cellTs bookmarkStore $ \(ts,_) ->
      [ #text := T.pack $ showFFloat (Just 6) (fromIntegral ts / 1000000) "s" ]

    cellLayoutSetAttributes columnLabel cellLabel bookmarkStore $ \(_,label) ->
      [ #text := label ]

    ---------------------------------------------------------------------------

    addBookmarkButton    <- getWidget ToolButton "add_bookmark_button"
    deleteBookmarkButton <- getWidget ToolButton "delete_bookmark"
    gotoBookmarkButton   <- getWidget ToolButton "goto_bookmark_button"

    on addBookmarkButton #clicked bookmarkViewAddBookmark

    on deleteBookmarkButton #clicked $ do
      (selected, _model, iter) <- treeSelectionGetSelected selection
      when selected $ do
        pos <- seqStoreIterToIndex iter
        bookmarkViewRemoveBookmark pos

    on gotoBookmarkButton #clicked $ do
      (selected, _model, iter) <- treeSelectionGetSelected selection
      when selected $ do
        pos <- seqStoreIterToIndex iter
        (ts, _) <- seqStoreGetValue bookmarkStore pos
        bookmarkViewGotoBookmark ts

    on bookmarkTreeView #rowActivated $ \path _ -> do
      indices <- treePathGetIndices path
      for_ (join $ listToMaybe <$> indices) $ \pos -> do
        (ts, _) <- seqStoreGetValue bookmarkStore pos
        bookmarkViewGotoBookmark ts

    set cellLabel [ #editable := True ]
    on cellLabel #edited bookmarkViewEditLabel

    ---------------------------------------------------------------------------

    return BookmarkView{..}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module GUI.EventsView (
    EventsView,
    eventsViewNew,
    EventsViewActions(..),

    eventsViewSetEvents,

    eventsViewGetCursor,
    eventsViewSetCursor,
    eventsViewScrollToLine,
  ) where

import Control.Monad.Reader
import Data.Int (Int32)
import Data.IORef
import Numeric

import Data.Array
import GHC.RTS.Events
import GI.Cairo.Render.Connector (renderWithContext)
import GI.Cairo.Render
import GI.Gtk
import GI.Pango as Pango
import qualified Data.Text as T
import qualified GI.Cairo as Cairo
import qualified GI.Gdk as Gdk

import GUI.GtkExtras (getWidgetAs)

-------------------------------------------------------------------------------

data EventsView = EventsView {
       drawArea :: !DrawingArea,
       adj      :: !Adjustment,
       stateRef :: !(IORef ViewState)
     }

data EventsViewActions = EventsViewActions {
       eventsViewCursorChanged :: Int -> IO ()
     }

data ViewState = ViewState {
       lineHeight  :: !Double,
       eventsState :: !EventsState
     }

data EventsState
   = EventsEmpty
   | EventsLoaded {
       cursorPos :: !Int,
       mrange    :: !(Maybe (Int, Int)),
       eventsArr :: Array Int Event
     }

-------------------------------------------------------------------------------

eventsViewNew :: Builder -> EventsViewActions -> IO EventsView
eventsViewNew builder EventsViewActions{..} = do

  stateRef <- newIORef undefined
  drawArea <- getWidgetAs builder "eventsDrawingArea" DrawingArea
  vScrollbar <- getWidgetAs builder "eventsVScroll" VScrollbar
  adj <- get vScrollbar #adjustment

  -- make the background white
  -- widgetModifyBg drawArea StateNormal (Color 0xffff 0xffff 0xffff) -- FIXME
  widgetSetCanFocus drawArea True
  --TODO: needs to be reset on each style change ^^

  -----------------------------------------------------------------------------
  -- Line height

  -- Calculate the height of each line based on the current font
  let getLineHeight = do
        pangoCtx <- widgetGetPangoContext drawArea
        fontDesc <- contextGetFontDescription pangoCtx
        metrics  <- contextGetMetrics pangoCtx (Just fontDesc) Nothing
        ascent <- fontMetricsGetAscent metrics
        descent <- fontMetricsGetDescent metrics
        return $! fromIntegral $ ascent + descent --TODO: padding?

  -- We cache the height of each line
  initialLineHeight <- getLineHeight
  -- but have to update it when the font changes
  on drawArea #styleSet $ \_ -> do
    lineHeight' <- getLineHeight
    modifyIORef' stateRef $ \viewstate -> viewstate { lineHeight = lineHeight' }

  -----------------------------------------------------------------------------

  writeIORef stateRef ViewState {
    lineHeight  = initialLineHeight,
    eventsState = EventsEmpty
  }

  let eventsView = EventsView {..}

  -----------------------------------------------------------------------------
  -- Drawing

  on drawArea #draw $ \context -> do
    state <- readIORef stateRef
    renderWithContext (drawEvents context eventsView state) context
    return True

  -----------------------------------------------------------------------------
  -- Key navigation

  on drawArea #keyPressEvent $ \event -> do
    let scroll by = do
          ViewState{eventsState, lineHeight} <- readIORef stateRef
          pagesize <- get adj adjustmentPageSize
          let pagejump = max 1 (truncate (pagesize / lineHeight) - 1)
          case eventsState of
            EventsEmpty                        -> return ()
            EventsLoaded{cursorPos, eventsArr} ->
                eventsViewCursorChanged cursorPos'
              where
                cursorPos'    = clampBounds range (by pagejump end cursorPos)
                range@(_,end) = bounds eventsArr
          return True

    key <- get event #keyval
    case key of
      Gdk.KEY_Up -> scroll (\_page _end pos -> pos - 1)
      Gdk.KEY_Down -> scroll (\_page _end pos -> pos + 1)
      Gdk.KEY_Page_Up -> scroll (\page _end pos -> pos - page)
      Gdk.KEY_Page_Down -> scroll (\page _end pos -> pos + page)
      Gdk.KEY_Home -> scroll (\_page _end _pos -> 0)
      Gdk.KEY_End -> scroll (\_page end _pos -> end)
      Gdk.KEY_Left -> return True
      Gdk.KEY_Right -> return True
      _ -> return False

  -----------------------------------------------------------------------------
  -- Scrolling

  set adj [ #lower := 0 ]

  on drawArea #sizeAllocate $ \_ ->
    updateScrollAdjustment eventsView =<< readIORef stateRef

  let hitpointToLine :: ViewState -> Double -> Double -> Maybe Int
      hitpointToLine ViewState{eventsState = EventsEmpty} _ _  = Nothing
      hitpointToLine ViewState{eventsState = EventsLoaded{eventsArr}, lineHeight}
                     yOffset eventY
        | hitLine > maxIndex = Nothing
        | otherwise          = Just hitLine
        where
          hitLine  = truncate ((yOffset + eventY) / lineHeight)
          maxIndex = snd (bounds eventsArr)

  on drawArea #buttonPressEvent $ \event -> do
    y  <- get event #yRoot
    viewState <- readIORef stateRef
    yOffset <- get adj #value
    widgetGrabFocus drawArea
    case hitpointToLine viewState yOffset y of
      Nothing -> return ()
      Just n  -> eventsViewCursorChanged n
    return True

  on drawArea #scrollEvent $ \event -> do
    dir <- get event #direction
    val      <- get adj #value
    upper    <- get adj #upper
    pagesize <- get adj #pageSize
    step     <- get adj #stepIncrement
    case dir of
      Gdk.ScrollDirectionUp -> set adj [ #value := val - step ]
      Gdk.ScrollDirectionDown ->
        set adj [ #value := min (val + step) (upper - pagesize) ]
      _ -> return ()
    return True

  on adj #valueChanged $
    widgetQueueDraw drawArea

  -----------------------------------------------------------------------------

  return eventsView

-------------------------------------------------------------------------------

eventsViewSetEvents :: EventsView -> Maybe (Array Int Event) -> IO ()
eventsViewSetEvents eventWin@EventsView{drawArea, stateRef} mevents = do
  viewState <- readIORef stateRef
  let eventsState' = case mevents of
        Nothing     -> EventsEmpty
        Just events -> EventsLoaded {
                          cursorPos  = 0,
                          mrange = Nothing,
                          eventsArr  = events
                       }
      viewState' = viewState { eventsState = eventsState' }
  writeIORef stateRef viewState'
  updateScrollAdjustment eventWin viewState'
  widgetQueueDraw drawArea

-------------------------------------------------------------------------------

eventsViewGetCursor :: EventsView -> IO (Maybe Int)
eventsViewGetCursor EventsView{stateRef} = do
  ViewState{eventsState} <- readIORef stateRef
  case eventsState of
    EventsEmpty             -> return Nothing
    EventsLoaded{cursorPos} -> return (Just cursorPos)

eventsViewSetCursor :: EventsView -> Int -> Maybe (Int, Int) -> IO ()
eventsViewSetCursor eventsView@EventsView{drawArea, stateRef} n mrange = do
  viewState@ViewState{eventsState} <- readIORef stateRef
  case eventsState of
    EventsEmpty             -> return ()
    EventsLoaded{eventsArr} -> do
      let n' = clampBounds (bounds eventsArr) n
      writeIORef stateRef viewState {
        eventsState = eventsState { cursorPos = n', mrange }
      }
      eventsViewScrollToLine eventsView  n'
      widgetQueueDraw drawArea

eventsViewScrollToLine :: EventsView -> Int -> IO ()
eventsViewScrollToLine EventsView{adj, stateRef} n = do
  ViewState{lineHeight} <- readIORef stateRef
  -- make sure that the range [n..n+1] is within the current page:
  adjustmentClampPage adj
    (fromIntegral  n    * lineHeight)
    (fromIntegral (n+1) * lineHeight)

-------------------------------------------------------------------------------

updateScrollAdjustment :: EventsView -> ViewState -> IO ()
updateScrollAdjustment EventsView{drawArea, adj}
                       ViewState{lineHeight, eventsState} = do

  (_,windowHeight) <- widgetGetSizeRequest drawArea
  let numLines = case eventsState of
                   EventsEmpty             -> 0
                   EventsLoaded{eventsArr} -> snd (bounds eventsArr) + 1
      linesHeight = fromIntegral numLines * lineHeight
      upper       = max linesHeight (fromIntegral windowHeight)
      pagesize    = fromIntegral windowHeight

  set adj [
       adjustmentUpper         := upper,
       adjustmentPageSize      := pagesize,
       adjustmentStepIncrement := pagesize * 0.2,
       adjustmentPageIncrement := pagesize * 0.9
    ]
  val <- get adj adjustmentValue
  when (val > upper - pagesize) $
    set adj [ adjustmentValue := max 0 (upper - pagesize) ]

-------------------------------------------------------------------------------

drawEvents :: Cairo.Context -> EventsView -> ViewState -> Render ()
drawEvents _ _ ViewState {eventsState = EventsEmpty} = return ()
drawEvents context EventsView {drawArea, adj}
           ViewState {lineHeight, eventsState = EventsLoaded{..}} = do

  yOffset    <- get adj #value
  pageSize   <- get adj #pageSize

  -- calculate which lines are visible
  let lower = truncate (yOffset / lineHeight)
      upper = ceiling ((yOffset + pageSize) / lineHeight)

      -- the array indexes [begin..end] inclusive
      -- are partially or fully visible
      begin = lower
      end   = min upper (snd (bounds eventsArr))

  styleCtx <- widgetGetStyleContext drawArea

  pangoCtx <- widgetGetPangoContext drawArea
  layout   <- Pango.layoutNew pangoCtx
  layoutSetEllipsize layout EllipsizeModeEnd

  clipRect <- widgetGetAllocation drawArea
  width <- get clipRect #width

  let -- With average char width, timeWidth is enough for 24 hours of logs
      -- (way more than TS can handle, currently). Aligns nicely with
      -- current timeline_yscale_area width, too.
      -- TODO: take timeWidth from the yScaleDrawingArea width
      -- TODO: perhaps make the timeWidth area grey, too?
      -- TODO: perhaps limit scroll to the selected interval (perhaps not strictly, but only so that the interval area does not completely vanish from the visible area)?
      timeWidth  = 105
      columnGap  = 20
      descrWidth :: Int32
      descrWidth = width - timeWidth - columnGap

  sequence_
    [ do
        when (inside || selected) $ do
          renderFrame styleCtx context 0 y (fromIntegral width) lineHeight
          renderBackground styleCtx context 0 y (fromIntegral width) lineHeight

          -- The event time
          layoutSetText layout (T.pack $ showEventTime event) (-1)
          layoutSetAlignment layout AlignmentRight
          layoutSetWidth layout timeWidth
          renderLayout styleCtx context 0 y layout

          -- The event description text
          layoutSetText layout (T.pack $ showEventDescr event) (-1)
          layoutSetAlignment layout AlignmentLeft
          layoutSetWidth layout descrWidth
          renderLayout styleCtx context (fromIntegral $ timeWidth + columnGap) y layout

    | n <- [begin..end]
    , let y = fromIntegral n * lineHeight - yOffset
          event    = eventsArr ! n
          inside   = maybe False (\ (s, e) -> s <= n && n <= e) mrange
          selected = cursorPos == n
    ]
  where
    showEventTime (Event time _spec _) =
      showFFloat (Just 6) (fromIntegral time / 1000000) "s"
    showEventDescr :: Event -> String
    showEventDescr (Event _time  spec cap) =
        (case cap of
          Nothing -> ""
          Just c  -> "HEC " ++ show c ++ ": ")
     ++ case spec of
          UnknownEvent{ref} -> "unknown event; " ++ show ref
          Message     msg   -> msg
          UserMessage msg   -> msg
          _                 -> showEventInfo spec

-------------------------------------------------------------------------------

clampBounds :: Ord a => (a, a) -> a -> a
clampBounds (lower, upper) x
  | x <= lower = lower
  | x >  upper = upper
  | otherwise  = x

module Lib
    ( videoPlayer
    , Player (..)
    ) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.Enums
import Media.Streaming.GStreamer
import Media.Streaming.GStreamer.XOverlay
import Media.Streaming.GStreamer.Core.Event
import System.Glib
import System.Glib.Properties

import Control.Monad.Trans (liftIO)
import Control.Monad (void, when)
import Data.Text (Text (..), intercalate, justifyRight, pack, append)
import Data.IORef
import Data.Word
import Control.Concurrent (forkIO)

data Player = Player
  { source :: Element
  , container :: Container
  , pipeline :: Element
  }

type Test = Drawable

videoPlayer :: Player -> IO ()
videoPlayer player = do
  main <- vBoxNew False 0
  fullscreenWindow <- windowNew
  windowFullscreen fullscreenWindow

  draw <- drawingAreaNew
  containerAdd (container player) main

  widgetModifyBg draw StateNormal (Color 0 0 0)


  controls   <- hBoxNew False 10
  play       <- toggleButtonNewWithLabel "Play/Pause"
  position   <- labelNew (Just "0:00:00")
  duration   <- labelNew (Just "0:00:00")
  timeSlider <- hScrollbarNewDefaults
  volume     <- volumeButtonNew
  fullscreen <- buttonNewWithLabel "Fullscreen"

  volume `set` [scaleButtonValue := 1]

  onClicked play . playPause $ pipeline player
  onClicked fullscreen $ toggleFullscreen (container player) main fullscreenWindow

  boxPackStart main     draw       PackGrow    0
  boxPackEnd   main     controls   PackNatural 0
  boxPackStart controls play       PackNatural 0
  boxPackStart controls position   PackNatural 0
  boxPackStart controls timeSlider PackGrow    0
  boxPackStart controls duration   PackNatural 0
  boxPackStart controls volume     PackNatural 0
  boxPackEnd   controls fullscreen PackNatural 0

  seeking <- newIORef False
  motionTracker <- newIORef 0

  flip timeoutAdd 100 $ do
    seeking' <- readIORef seeking
    if not seeking' then do
      mPosition <- elementQueryPosition (pipeline player) FormatTime
      mDuration <- elementQueryDuration (pipeline player) FormatTime
      case mPosition of
        Just (_, position') ->
          case mDuration of
            Just (_, duration') -> do
              rangeSetValue timeSlider (fromIntegral position' / fromIntegral second)
              rangeSetRange timeSlider 0 (fromIntegral duration' / fromIntegral second)
              labelSetText position (formatTime position')
              labelSetText duration (formatTime duration')
              return True
            Nothing -> return True
        Nothing -> return True
    else return True

  timeSlider `on` changeValue $ \t i -> do
    -- First we enform all other functions that we are seeking,
    -- that means the slider value should not be changed
    writeIORef seeking True

    -- Then we set the slider to the new value
    rangeSetValue timeSlider i

    -- I have taken an arbirary 100 millisecond timeout here
    flip timeoutAdd 100 $ do
      thenVal <- rangeGetValue timeSlider

      -- thenVal == i if and only if the user has not moved in 100 milliseconds
      when (thenVal == i) . void $ do
        elementSeekSimple (source player)
                          FormatTime
                          [SeekFlagFlush, SeekFlagKeyUnit]
                          (round (i * fromIntegral second))
        writeIORef seeking False
      return False
    return True

  volume `on` scaleButtonValueChanged $ \t -> do
    objectSetPropertyDouble "volume" (source player) t

  widgetAddEvents draw [PointerMotionMask]

  elementSetState (pipeline player) StatePaused

  widgetShowAll main

  drawWindow <- widgetGetDrawWindow draw

  draw `on` motionNotifyEvent $ liftIO $ do
    drawWindowSetCursor drawWindow Nothing
    nowMotion <- readIORef motionTracker
    writeIORef motionTracker (nowMotion + 1)
    widgetShowAll controls
    flip timeoutAdd 2000 $ do
      thenMotion <- readIORef motionTracker
      when (thenMotion == nowMotion + 1) . void $ do
        widgetHide controls
        drawWindowSetCursor drawWindow =<< (Just <$> cursorNew BlankCursor)
        writeIORef motionTracker 0
      return False
    return True

  handle <- drawableGetID drawWindow
  xOverlaySetWindowHandle (source player) handle
  return ()

playPause :: Element -> IO ()
playPause pipe = do
  currentState <- elementGetState pipe clockTimeNone
  case currentState of
    (_, Just StatePlaying, _) -> void $ elementSetState pipe StatePaused
    (_, Just StatePaused, _) -> void $ elementSetState pipe StatePlaying
    _ -> return ()

toggleFullscreen :: Container -> VBox -> Window -> IO ()
toggleFullscreen con wgt wnd = do
  isVisible <- widgetGetVisible wnd
  if not isVisible then do
    widgetShowAll wnd
    widgetReparent wgt wnd
  else do
    widgetReparent wgt con
    widgetHide wnd
  return ()

formatTime :: ClockTime -> Text
formatTime n = intercalate (pack ":") [toHours n, toMinutes n, toSeconds n]

toSeconds :: ClockTime -> Text
toSeconds i = justifyRight 2 '0' . pack . show $ (i `div` 1000000000) `mod` 60

toMinutes :: ClockTime -> Text
toMinutes i = justifyRight 2 '0' . pack . show $ (i `div` 60000000000) `mod` 60

toHours :: ClockTime -> Text
toHours i = pack . show $ (i `div` 3600000000000) `mod` 60

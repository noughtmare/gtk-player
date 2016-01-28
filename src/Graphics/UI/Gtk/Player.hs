{-# OPTIONS_GHC -Wall #-}
module Graphics.UI.Gtk.Player
    ( playerNew
    , Player
    , PlayerData (..)
    ) where

import Graphics.UI.Gtk
--import Graphics.UI.Gtk.General.Enums
import Media.Streaming.GStreamer
import Media.Streaming.GStreamer.XOverlay
import Media.Streaming.GStreamer.Core.Event
--import System.Glib
import System.Glib.Properties

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Control.Monad (void, when)
import Data.Text (Text, intercalate, justifyRight, pack)
import Data.IORef
import Data.Word
import Data.Maybe (fromJust)
--import Control.Concurrent

data PlayerData = PD
  { source :: Element
  , pipeline :: Element
  }

type Player = VBox

playerNew :: PlayerData -> IO Player
playerNew player = do
  main <- vBoxNew False 0

  -- A seperate window is needed to fullscreen only the player itself
  fullscreenWindow <- windowNew
  windowFullscreen fullscreenWindow

  -- This is the widget that we'll draw the video on
  draw <- drawingAreaNew

  -- This prevents 'blinking' when resizing
  widgetSetRedrawOnAllocate draw False

  containerRef <- newIORef Nothing :: IO (IORef (Maybe Widget))

  -- This connects gstreamer with gtk when the window is shown
  void . afterShow main $ do
    writeIORef containerRef =<< widgetGetParent main
    drawWindow <- widgetGetDrawWindow draw
    handle <- drawableGetID drawWindow
    xOverlaySetWindowHandle (source player) handle
    return ()

  controls   <- eventBoxNew
  controlBox <- hBoxNew False 10
  play       <- toggleButtonNewWithLabel "Play/Pause"
  position   <- labelNew (Just "0:00:00")
  duration   <- labelNew (Just "0:00:00")
  timeSlider <- hScaleNewWithRange 0 1 (fromIntegral $ (5 :: Word64) * 10 ^ (9 :: Integer))
  volume     <- volumeButtonNew
  fullscreen <- buttonNewWithLabel "Fullscreen"

  containerAdd controls controlBox

  volume     `set` [scaleButtonValue := 1]
  timeSlider `set` [scaleDrawValue := False]

  void . onClicked play . playPause $ pipeline player
  void . onClicked fullscreen $ toggleFullscreen containerRef main fullscreenWindow

  boxPackStart main       draw       PackGrow    0
  boxPackEnd   main       controls   PackNatural 0
  boxPackStart controlBox play       PackNatural 0
  boxPackStart controlBox position   PackNatural 0
  boxPackStart controlBox timeSlider PackGrow    0
  boxPackStart controlBox duration   PackNatural 0
  boxPackStart controlBox volume     PackNatural 0
  boxPackEnd   controlBox fullscreen PackNatural 0

  seeking <- newIORef False
  motionTimeout <- newIORef Nothing :: IO (IORef (Maybe HandlerId))

  void . flip timeoutAdd 100 $ do
    seeking' <- readIORef seeking
    if not seeking' then do
      mPosition <- elementQueryPosition (pipeline player) FormatTime
      mDuration <- elementQueryDuration (pipeline player) FormatTime
      case mPosition of
        Just (_, position') ->
          case mDuration of
            Just (_, duration') -> do
              rangeSetValue timeSlider (fromIntegral position')
              rangeSetRange timeSlider 0 (fromIntegral duration')
              labelSetText position (formatTime position')
              labelSetText duration (formatTime duration')
              return True
            Nothing -> return True
        Nothing -> return True
    else return True

  void . on timeSlider changeValue $ \_ i -> do
    -- First we enform all other functions that we are seeking,
    -- that means the slider value should not be changed
    writeIORef seeking True

    -- Then we set the slider to the new value
    rangeSetValue timeSlider i

    -- I have taken an arbirary 100 millisecond timeout here
    void . flip timeoutAdd 100 $ do
      thenVal <- rangeGetValue timeSlider

      -- thenVal == i if and only if the user has not moved in 100 milliseconds
      when (thenVal == i) . void $ do
        void . elementSeekSimple (source player)
                                 FormatTime
                                 [SeekFlagFlush, SeekFlagKeyUnit]
                               $ round i
        writeIORef seeking False
      return False
    return True

  void . on volume scaleButtonValueChanged $
    objectSetPropertyDouble "volume" (source player)

  widgetAddEvents draw [PointerMotionMask]
  widgetAddEvents controls [PointerMotionMask]

  void $ elementSetState (pipeline player) StatePaused

  void . on draw     motionNotifyEvent $ motionNotifyHandler draw controls motionTimeout True
  void . on controls motionNotifyEvent $ motionNotifyHandler draw controls motionTimeout False
  return main

motionNotifyHandler :: MonadIO m => DrawingArea -> EventBox -> IORef (Maybe HandlerId) -> Bool -> m Bool
motionNotifyHandler draw controls motionTimeout hide = liftIO $ do
  drawWindow <- widgetGetDrawWindow draw
  drawWindowSetCursor drawWindow Nothing
  widgetShowAll controls

  maybe (return ()) timeoutRemove =<< readIORef motionTimeout
  if hide then
    writeIORef motionTimeout . Just =<< timeoutAdd
      (do widgetHide controls
          drawWindowSetCursor drawWindow =<< (Just <$> cursorNew BlankCursor)
          writeIORef motionTimeout Nothing
          return False)
      2000
  else
    writeIORef motionTimeout Nothing
  return True

playPause :: Element -> IO ()
playPause pipe = do
  currentState <- elementGetState pipe clockTimeNone
  case currentState of
    (_, Just StatePlaying, _) -> void $ elementSetState pipe StatePaused
    (_, Just StatePaused, _) -> void $ elementSetState pipe StatePlaying
    _ -> return ()

toggleFullscreen :: IORef (Maybe Widget) -> Player -> Window -> IO ()
toggleFullscreen con wgt wnd = do
  isVisible <- widgetGetVisible wnd
  if not isVisible then do
    widgetShowAll wnd
    widgetReparent wgt wnd
  else do
    widgetReparent wgt . fromJust =<< readIORef con
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

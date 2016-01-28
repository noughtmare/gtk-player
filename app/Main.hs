module Main where

import Media.Streaming.GStreamer as Gst
import System.Glib.Properties
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Player

import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)
import Control.Monad (void)

main :: IO ()
main = do
  void initGUI
  Gst.init

  window <- windowNew
  testBox <- vBoxNew True 0

  containerAdd window testBox

  pipe <- pipelineNew "xoverlay"
  src <- fromJust <$> elementFactoryMake "playbin2" (Just "source")
  void $ binAdd (castToPipeline pipe) src

  objectSetPropertyString "uri" src "http://docs.gstreamer.com/media/sintel_trailer-480p.webm"

  void . on window deleteEvent . liftIO $ mainQuit >> return False

  widgetShowAll window -- This shouldn't be necessary (BUT IT IS!)

  player <- playerNew PD { source = src, pipeline = pipe }

  containerAdd testBox player
  widgetShowAll player

  mainGUI

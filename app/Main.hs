module Main where

import Media.Streaming.GStreamer as Gst
import System.Glib.Properties as G
import Graphics.UI.Gtk
import Lib

import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  initGUI
  Gst.init

  window <- windowNew
  testBox <- vBoxNew True 0
  --testText <- labelNew (Just "test")

  containerAdd window testBox
  --containerAdd testBox testText

  pipeline <- pipelineNew "xoverlay"
  src <- fromJust <$> elementFactoryMake "playbin2" (Just "source")
  --sink <- fromJust <$> elementFactoryMake "xvimagesink" (Just "sink")
  mapM_ (binAdd (castToPipeline pipeline)) [src]--,sink]
  --elementLink src sink

  G.objectSetPropertyString "uri" src "http://www.sample-videos.com/video/mp4/240/big_buck_bunny_240p_50mb.mp4"

  window `on` deleteEvent $ liftIO mainQuit >> return False
  widgetShowAll window

  videoPlayer Player { source = src --sink
                     , container = toContainer testBox
                     , pipeline = pipeline
                     }

  mainGUI

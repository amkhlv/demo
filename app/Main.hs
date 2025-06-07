{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless, void)
import Data.GI.Base
import GI.GObject.Objects.Object
import GI.Gio.Objects.Cancellable
import qualified GI.Gio.Objects.Task as GTask
import qualified GI.Gtk as Gtk
import System.Exit (ExitCode (ExitSuccess))
import qualified System.Process as SysProc

activate :: Gtk.Application -> IO ()
activate app = do
  hbox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 1]

  window <-
    new
      Gtk.ApplicationWindow
      [ #application := app,
        #title := "example",
        #child := hbox
      ]
  buttonOpenPdQ <-
    new
      Gtk.Button
      [ #label := "press to crash me",
        On #clicked $ do
          putStrLn "-- preparing taskNew"
          task <- GTask.taskNew (Nothing :: Maybe Object) (Nothing :: Maybe Cancellable) Nothing
          putStrLn "-- starting in thread"
          GTask.taskRunInThread task $ \_tsk _obj _dat _mcanc -> do
            (_, _, _, handle) <- SysProc.createProcess $ SysProc.proc "gvim" ["/tmp"]
            code <- SysProc.waitForProcess handle
            unless (code == ExitSuccess) $ print code
          putStrLn "-- started in thread"
      ]
  toolbar <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 1]
  toolbar.append buttonOpenPdQ

  hbox.append toolbar
  Gtk.windowSetDefaultSize window 400 400
  window.show

main :: IO ()
main = do
  app <-
    new
      Gtk.Application
      [ #applicationId := "TaskExample",
        On #activate (activate ?self)
      ]
  void $ app.run Nothing

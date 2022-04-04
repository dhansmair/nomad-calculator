{-
   Main GUI application.

    code inspired by lecture script in gtk2hs
-}

-- own imports

import Builtins (stdEnv)
import Control.Monad
import Data.IORef
import Environment (Env)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Interpreter

-- container for references to gui objects
data MainGUI = MainGUI
  { window :: Window,
    addButton :: Button,
    entry_0 :: Entry,
    menu_quit :: MenuItem,
    textbuffer_log :: TextBuffer
  }

-- function to load a MainGUI containing all the references to
-- objects in the window
loadGUI = do
  builder <- builderNew
  builderAddFromFile builder "./static/gui-draft.glade"

  window <- builderGetObject builder castToWindow "window1"
  addButton <- builderGetObject builder castToButton "button_add"
  entry_0 <- builderGetObject builder castToEntry "entry_0"
  menu_quit <- builderGetObject builder castToMenuItem "menu_quit"
  textbuffer_log <- builderGetObject builder castToTextBuffer "textbuffer_log"

  return $ MainGUI window addButton entry_0 menu_quit textbuffer_log

handleAddButton :: MainGUI -> IORef Env -> IO ()
handleAddButton gui state = do
  putStrLn "add button pressed."
  textBufferSetText (textbuffer_log gui) "add button pressed"

handleEnter :: MainGUI -> IORef Env -> IO ()
handleEnter gui state = do
  putStrLn "enter pressed"
  textBufferSetText (textbuffer_log gui) "enter pressed"
  text <- entryGetText (entry_0 gui)
  entrySetText (entry_0 gui) $ reverse text

-- main program
main :: IO ()
main = do
  initGUI
  gui <- loadGUI

  -- define the "model"
  state <- newIORef stdEnv

  -- event listeners and callbacks
  on (addButton gui) buttonActivated $ handleAddButton gui state
  on (entry_0 gui) entryActivated $ handleEnter gui state
  -- on (enterButton gui) buttonActivated $ readInput gui state
  -- on (input gui) entryBackspace $ readInput gui state             -- TODO how to catch an enter press event?
  on (window gui) objectDestroy mainQuit
  on (menu_quit gui) menuItemActivated mainQuit

  -- TODO add more event listeners

  widgetShowAll (window gui)
  mainGUI

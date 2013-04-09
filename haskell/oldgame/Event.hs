module Event (start, initialize) where

import Graphics.UI.GLUT hiding (initialize)

type Initializer = String -> [String] -> IO (Maybe IdleCallback)

start :: Initializer -> IO ()
start onInit = do
  initialize onInit
  mainLoop

initialize :: Initializer -> IO ()
initialize onInit = do
  (name, args) <- getArgsAndInitialize
  actionOnWindowClose $= ContinueExectuion --I wish this was spelled correctly.
  onUpdate <- onInit name args
  idleCallback $= onUpdate

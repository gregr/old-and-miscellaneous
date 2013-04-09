module Input (setInputHandlers, Key(..), KeyState(..), Position(..)) where

import Graphics.UI.GLUT

setInputHandlers :: (Maybe KeyboardMouseCallback ->
                     Maybe MotionCallback ->
                     IO ())
setInputHandlers onButton onMotion = do
  keyboardMouseCallback $= onButton
  motionCallback $= onMotion
  passiveMotionCallback $= onMotion

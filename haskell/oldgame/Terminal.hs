module Terminal (terminal, Key(..), KeyState(..)) where

import Display

terminal :: (String -> Size -> DisplayCallback -> Maybe ReshapeCallback ->
             Maybe KeyboardMouseCallback -> Maybe MotionCallback -> IO Window)
terminal display onButton onMotion = do
  w <- display name size onDisplay onReshape
  keyboardMouseCallback $= onButton
  motionCallback $= onMotion
  passiveMotionCallback $= onMotion

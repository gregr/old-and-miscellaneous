module Display (display, renderer, Size(..)) where

import Graphics.UI.GLUT hiding (renderer)

display :: (String -> Size -> DisplayCallback -> Maybe ReshapeCallback ->
            IO Window)
display name size onDisplay onReshape = do
  initialDisplayMode $= [DoubleBuffered, RGBAMode, WithDepthBuffer
                        ,WithAlphaComponent]
  initialWindowSize $= size
  w <- createWindow name
  displayCallback $= onDisplay
  reshapeCallback $= onReshape
  return w

renderer :: IO () -> DisplayCallback
renderer onRender = do
  clear [ColorBuffer, DepthBuffer]
  onRender
  swapBuffers

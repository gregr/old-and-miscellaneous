module Main where

import Event
import Display
import Input

onDisplay = renderer $ return ()

onInit name args = do
  print "initializing"
  t <- display name (Size 800 600) onDisplay Nothing
  setInputHandlers (Just onButton) Nothing
  ts <- mapM (\name -> display name (Size 640 480) onDisplay Nothing) args
  setInputHandlers Nothing $ Just onMotion
  print name
  print args
  return $ Just onUpdate

onUpdate = return ()

--onButton (Char c) Down _ _ = print c
onButton button Down _ _ = print button
onButton button state modifiers position = return ()

onMotion (Position x y) = print $ "mouse position: " ++ show x ++ "," ++ show y

main = start onInit

import Event
import Display
import Input
import Object

import Data.Time.Clock
import Data.IORef

onInit name args = do
  print $ name ++ ": " ++ unwords args
  display name (Size 400 400) onDisplay Nothing
  ship <- newIORef object
  setInputHandlers (Just $ onButton ship) Nothing
  return $ Just onUpdate

onUpdate ship = do
  dt <- getSeconds
  update ship dt
  print ship

onDisplay = renderer $ return ()

onButton obj _ _ _ _ = return ()

getSeconds = do
  UTCTime _ s <- getClockTime
  return s

--onButton (Char c) Down _ _ = print c
--onButton button Down _ _ = print button
--onButton button state modifiers position = return ()

--onMotion (Position x y) = print $ "mouse position: " ++ show x ++ "," ++ show y

-- timeStep = IORef 0

-- timeIsZero = do
--   t <- timeStep
--   return $ t == 0

-- setTimeStep t threshold =
--     writeIORef timeStep (if t > threshold then threshold else t)

-- tick dt threshold = do
--   t <- readIORef timeStep
--   setTimeStep t + dt threshold

turnLeft obj = turn obj 0.01
turnRight obj = turn obj -0.01

thrustForward obj = thrust obj 0.01
thrustBackward obj = thrust obj -0.01

turn obj angle = modifyIORef obj (flip rotate angle)

thrust obj force = modifyIORef obj (flip propel force)

update obj dt = modifyIORef obj drift dt

main = start onInit

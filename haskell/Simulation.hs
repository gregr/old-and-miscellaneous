{-# LANGUAGE FlexibleContexts #-}
module Simulation where

import Physics
import Vector

data Dynamic v av s = Dynamic {kinetic :: Kinetic s,
                              linearKinematic :: Kinematic (v s),
                              angularKinematic :: Kinematic (av s)}
                      deriving (Show, Eq)

data Static s = Static {kinetic2 :: Kinetic s}
                deriving (Show, Eq)

data Kinematic v = Kinematic {position, velocity, acceleration :: v}
                   deriving (Show, Eq)

--TODO: inertia, forces, loads, etc.
data Kinetic s = Kinetic {mass :: s} deriving (Show, Eq)

--TODO: Dynamic vs. Static bodies
-- data Body v av s = Body {shape :: Shape v s,
--                          kinetic :: Kinetic s,
--                          linearKinematic :: Kinematic (v s),
--                          angularKinematic :: Kinematic (av s)}
--                    deriving (Show, Eq)

linearPosition = position . linearKinematic
linearVelocity = velocity . linearKinematic
linearAcceleration = acceleration . linearKinematic

angularPosition = position . angularKinematic
angularVelocity = velocity . angularKinematic
angularAcceleration = acceleration . angularKinematic

-- testBody = (Body (AABB (Vector2 2 2)) (Kinetic 1.5)
--             (Kinematic (Vector2 0 0) (uniform 10) (uniform 0))
--             (Kinematic (Vector1 0) (uniform 0) (uniform 0)))

momentum body = scale (mass (kinetic body)) (linearVelocity body)

-- collisionBody a b = (collision
--                      (shape a) (linearPosition a) (angularPosition a)
--                      (shape b) (linearPosition b) (angularPosition b))

{- simulation 
--verletFirst -> new position and half-step velocity
--handle Collisions -> fix position and half-step velocity
--calc accelerations based on new positions -> a'
--verletSecond -> new velocity
--apply terminal velocity and calc drag accelerations
-}

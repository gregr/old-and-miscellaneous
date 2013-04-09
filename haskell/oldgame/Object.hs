module Object where

type Vector = (Float, Float)
type Angle = Float
type Mass = Float

data Object = Object { pos, vel :: Vector,
                       orient :: Angle,
                       mass :: Mass }
              deriving (Show)

object = Object (0, 0) (0, 0) 0 1

add (x, y) (xx, yy) = (x+xx, y+yy)

mult (x, y) s = (s*x, s*y)

drift obj@(Object p v _ _) dt = obj { pos = add p $ mult v dt }

propel obj@(Object _ v _ _) dv = obj { vel = add v dv }

rotate obj@(Object _ _ o _) angle = obj { orient = o + angle}

angleToVector angle = (cos angle, sin angle)

thrust obj@(Object _ v o m) impulse = propel obj dv
    where dv = (mag*ox, mag*oy)
              where (ox, oy) = angleToVector o
                    mag = impulse / m

module Physics where

type Scalar = Float
type Vector = (Scalar, Scalar, Scalar)

zero = (0, 0, 0)

add (x, y, z) (xx, yy, zz) = (x+xx, y+yy, z+zz)

sub a b = add a $ scale (-1) b

scale s (x, y, z) = (s*x, s*y, s*z)

dot (i, j, k) (ii, jj, kk) = i*ii + j*jj + k*kk

cross (i, j, k) (ii, jj, kk) = (j*kk -jj*k, k*ii-kk*i, i*jj-ii*j)

project a b = scale ((dot a b) / (dot b b)) b

magnitude v = sqrt $ dot v v

normalize v = scale (1/(magnitude v)) v

-- for 2D vectors
xyRightNormal (x, y, _) = (y, -x, 0)

zxRightNormal (x, _, z) = (-z, 0, x)

yzRightNormal (_, y, z) = (0, z, -y)

perpProduct a b = dot a $ xyRightNormal b



data Kinematic = Kinematic { position, velocity :: Vector }
                 deriving (Show, Eq)

kinematic = Kinematic zero zero

simulateKinematic (Kinematic s v) a dt =
    let adt = scale dt a
        vdt = scale dt v
    in let adt2 = scale (0.5*dt) adt
       in Kinematic (add s $ add vdt adt2) (add v adt)

data Body = Body { linear, angular :: Kinematic,
                   inertia :: Vector,
                   mass :: Scalar }
            deriving (Show, Eq)

--simulateBody (Body l a m i) force torque dt =
--    Body { linear = simulateKinematic l force/m dt,
--           angular = simulateKinematic a torque/i dt }

--inertia m r = m*r*r

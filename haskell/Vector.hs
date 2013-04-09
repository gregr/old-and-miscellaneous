{-# OPTIONS -fglasgow-exts #-}
module Vector where

data Vector1 a = Vector1 !a deriving (Show, Eq)
data Vector2 a = Vector2 !a !a deriving (Show, Eq)
data Vector3 a = Vector3 !a !a !a deriving (Show, Eq)

class Vector v where
    uniform :: s -> v s
    scale :: (Num s) => s -> v s -> v s
    add :: (Num s) => v s -> v s -> v s
    sub :: (Num s) => v s -> v s -> v s
    dot :: (Num s) => v s -> v s -> s

    magnitude :: (Vector v, Floating s) => v s -> s
    normalize :: (Vector v, Floating s) => v s -> v s
    project :: (Vector v, Fractional s) => v s -> v s -> v s

    magnitude v = sqrt $ dot v v
    normalize v = scale (1/(magnitude v)) v
    project a b = scale ((dot a b) / (dot b b)) b

--    sub v w = add v $ scale (-1) w --probably not very efficient...?

instance Vector Vector1 where
    uniform s = Vector1 s
    scale s (Vector1 x) = Vector1 (s*x)
    add (Vector1 x) (Vector1 xx) = Vector1 (x+xx)
    sub (Vector1 x) (Vector1 xx) = Vector1 (x-xx)
    dot (Vector1 i) (Vector1 ii) = i*ii

    magnitude (Vector1 x) = x
    normalize _ = Vector1 1
    project a _ = a

instance Vector Vector2 where
    uniform s = Vector2 s s
    scale s (Vector2 x y) = Vector2 (s*x) (s*y)
    add (Vector2 x y) (Vector2 xx yy) = Vector2 (x+xx) (y+yy)
    sub (Vector2 x y) (Vector2 xx yy) = Vector2 (x-xx) (y-yy)
    dot (Vector2 i j) (Vector2 ii jj) = i*ii + j*jj

instance Vector Vector3 where
    uniform s = Vector3 s s s
    scale s (Vector3 x y z) = Vector3 (s*x) (s*y) (s*z)
    add (Vector3 x y z) (Vector3 xx yy zz) = Vector3 (x+xx) (y+yy) (z+zz)
    sub (Vector3 x y z) (Vector3 xx yy zz) = Vector3 (x-xx) (y-yy) (z-zz)
    dot (Vector3 i j k) (Vector3 ii jj kk) = i*ii + j*jj + k*kk

cross :: (Num s) => Vector3 s -> Vector3 s -> Vector3 s
cross (Vector3 i j k) (Vector3 ii jj kk) =
    Vector3 (j*kk-jj*k) (k*ii-kk*i) (i*jj-ii*j)

-- is any of this necessary?
-- for 2D vectors
--xyRightNormal (x, y, _) = (y, -x, 0)
--zxRightNormal (x, _, z) = (-z, 0, x)
--yzRightNormal (_, y, z) = (0, z, -y)
--perpProduct a b = dot a $ xyRightNormal b

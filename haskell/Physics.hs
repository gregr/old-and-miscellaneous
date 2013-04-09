module Physics where

--kinematics

class Dynamic a where
    displace :: (Num t) => t -> t -> a -> a
    accelerate :: (Num t) => t -> a -> a


verletFirst s v a dt dtHalf = (s + v*dt + aterm*dt, v + aterm)
    where aterm = a*dtHalf

verletSecond v' a' dtHalf = v' + a'*dtHalf

{-
verletFirstBody dt dtHalf b@(Body _ _ (x, y) (vx, vy) (ax, ay)) =
    b{position=(x', y'), velocity=(vxHalf, vyHalf)}
        where (x', vxHalf) = verletFirst x vx ax dt dtHalf
              (y', vyHalf) = verletFirst y vy ay dt dtHalf

verletSecondBody dtHalf b@(Body _ _ _ (vxHalf, vyHalf) (ax, ay)) =
    b{velocity=(vx, vy)}
        where vx = verletSecond vxHalf ax dtHalf
              vy = verletSecond vyHalf ay dtHalf
-}
--kinetics

elasticVelocity m1 u1 m2 u2 e = (v1, v2)
    where v1 = (u1*(m1 - e*m2) + bounce*m2*u2) / msum
          v2 = (u2*(m2 - e*m1) + bounce*m1*u1) / msum
          msum = m1 + m2
          bounce = e+1

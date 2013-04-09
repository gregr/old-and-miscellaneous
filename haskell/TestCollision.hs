import Collision
import Physics

collide ma (wa, ha) (xa, ya) (vxa, vya) mb (wb, hb) (xb, yb) (vxb, vyb) e = do
  let xdiff = xa - xb
  xpen <- penetration xdiff wa wb
  let ydiff = ya - yb
  ypen <- penetration ydiff ha hb
  if xpen < ypen
    then results id xdiff xpen xa ya xb yb vxa vya vxb vyb
    else results flip ydiff ypen ya xa yb xb vya vxa vyb vxb
  where results modify diff pen x1a x2a x1b x2b v1a v2a v1b v2b = do
          proj <- return $ projection diff pen
          let partialProj = proj*mb/(ma+mb) --a hack, but what else is fair?
          let (v1a', v1b') = elasticVelocity ma v1a mb v1b e
          return (((pack (x1a+partialProj) x2a), (pack v1a' v2a)),
                  ((pack (x1b-proj+partialProj) x2b), (pack v1b' v2b)))
            where pack = modify (,)

{- simulation 
--verletFirst -> new position and half-step velocity
--handle Collisions -> fix position and half-step velocity
--calc accelerations based on new positions -> a'
--verletSecond -> new velocity
--apply terminal velocity and calc drag accelerations
-}

data Body = Body { mass :: Float,
                   halfSize,
                   position, velocity, acceleration :: (Float, Float) }
            deriving (Show, Eq)


collideBodies [] = []
collideBodies (body:bodies) = (bresult:(collideBodies rest))
    where (bresult, rest) =
              (foldl (\(b1, finished) b2 ->
                          let (br1, br2) = collideBody b1 b2 in
                          (br1, br2:finished))
               (body, []) bodies)

collideBody b1 b2 =
    case (collide (mass b1) (halfSize b1)
          (position b1) (velocity b1)
          (mass b2) (halfSize b2)
          (position b2) (velocity b2) 1) of --assume e=1
      Nothing -> (b1, b2)
      Just ((s1', v1'), (s2', v2')) -> (b1{position=s1', velocity=v1'},
                                        b2{position=s2', velocity=v2'})


test1 = Body 1 (3, 3) (0, 0) (10, 0) (0, 0)
test2 = Body 1 (3, 3) (20, 0.25) (-5, 0) (0, 0)
test3 = Body 1 (3, 3) (10, 0.5) (-10, 0) (0, 0)
test4 = Body 1 (3, 3) (10, 0.0) (5, 20) (0, -10)


tests1 = [test1, test2]
tests2 = [test1, test2, test3]
tests3 = [test4]

--begin in contact
test8 = Body 1 (2, 2) (0, 0) (10, 0) (0, 0)
test9 = Body 1 (2, 2) (3, 0) (-5, 0) (0, 0)

showBodies bodies = unlines $ map show bodies

printSim steps dt bodies =
    mapM_ putStrLn (map showBodies $ take steps $ iterate (simulate dt) bodies)

-- TODO: calc new accelerations, calc drag/terminal velocity
simulate dt bodies =
    (map (verletSecondBody dtHalf)
             $ collideBodies $ map (verletFirstBody dt dtHalf) bodies)
        where dtHalf = 0.5*dt

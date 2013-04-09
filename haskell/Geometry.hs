{- --Is this needed?
bodyToAxisWidth par perp cosAngle sinAngle = par*cosAngle + perp*sinAngle

testBodyToAxisWidth par perp angle = bodyToAxisWidth par perp cosa sina
    where cosa = abs $ cos angle
          sina = abs $ sin angle
-}

voronoiCorner ax ay bx by bw bh = do
  x <- voronoiSide ax bx bw
  y <- voronoiSide ay by bh
  return (x, y)

voronoiSide a b bw | (diff < (-bw)) = Just $ b - bw
                   | (diff > bw) = Just $ b + bw
                   | otherwise = Nothing
                   where diff = a - b

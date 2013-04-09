{-# LANGUAGE FlexibleContexts #-}
--{-# OPTIONS -fglasgow-exts #-}
module Collision where

import Vector
import Control.Monad

{- overlap functions for AABBs described by positions and half-widths -}

overlapAxis sa wa sb wb =
    liftM projection $ penetration
        where penetration | result > 0 = Just result
                          | otherwise  = Nothing
                  where result = wa + wb - (abs diff)
              projection pen = if diff < 0 then negate pen else pen
              diff = sa - sb

class Overlap v where
    overlap :: (Ord s, Num s) => v s -> v s -> v s -> v s -> Maybe (v s)

instance Overlap Vector1 where
    overlap (Vector1 xa) (Vector1 wa) (Vector1 xb) (Vector1 wb) =
        liftM Vector1 (overlapAxis xa wa xb wb)

instance Overlap Vector2 where
    overlap (Vector2 xa ya) (Vector2 wa ha) (Vector2 xb yb) (Vector2 wb hb) =
        liftM2 Vector2 (overlapAxis xa wa xb wb) (overlapAxis ya ha yb hb)

instance Overlap Vector3 where
    overlap (Vector3 xa ya za) (Vector3 wa ha ka)
            (Vector3 xb yb zb) (Vector3 wb hb kb) =
        (liftM3 Vector3 (overlapAxis xa wa xb wb)
                        (overlapAxis ya ha yb hb)
                        (overlapAxis za ka zb kb))

-- for now, assume any 3rd dimension is simply a projection of the 2d shape
data Shape v s = AABB (v s) | Circle s -- | Rectangle -- | Triangle
             deriving (Show, Eq)
--data Shape3d = AABB3d | Cylinder | Sphere

--data Geometry v = Geometry {shape :: Shape, aabbSize :: v} deriving (Show, Eq)

collision (AABB sizeA) centerA _ (AABB sizeB) centerB _ =
    overlap centerA sizeA centerB sizeB
collision _ _ _ _ _ _ = error "unsupported collision type"

-- class Collision b where
--     getGeometry :: b -> g (v s)
--     getCenter :: b -> (v s)
--     collision :: b -> b -> Maybe (v s)
--     collision a b = collisionGeom (getGeometry a) (getCenter a) (getGeometry b) (getCenter b)
--    collision :: b -> b -> Maybe (v s) --(Num s) => b v s -> b v s -> Maybe (v s)

type Scalar = Float


-- collision a b = uncurry (uncurry overlap (overlapArgs a)) (overlapArgs b)
--     where overlapArgs body = ((position (linearKinematic body)),
--                               (halfSize body))

-- instance Collide Body Vector2 where
--     collision a b = collisionBody (position a) (halfSize a) (position b) (halfSize b)

-- collisionBody a b = overlap2d (position a) (halfSize a) (position b) (halfSize b)

module Main where

--import Monad
import Array

squares a b = listArray (a, b) [x*x|x<-[a..b]]

makeArray f bnds = array bnds [(i, f i) | i <- range bnds]

grid f width height = makeArray f ((1, 1), (width, height))

testGrid = grid (\x -> '-') 5 5

--showGrid grid = let (_, (width, height)) = bounds grid in
--                pass

showVertically [] = ""
showVertically (x:xs) = shows x ('\n' : (showVertically xs))

showHorizontally xs = foldr shows "" xs

row grid width y = [grid ! (x, y) | x <- [1..width]]

getTestRow = row testGrid 5

joinVertically xs = foldr (\a b -> a ++ ('\n':b)) "" (map showHorizontally xs)

main = putStrLn $ joinVertically [getTestRow i | i <- [1..5]]

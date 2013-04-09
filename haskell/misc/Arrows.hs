import Control.Arrow (returnA, (>>>), arr)

idA :: a -> a
idA = proc a -> returnA -< a

plusOne :: Int -> Int
--plusOne = proc a -> returnA -< (a+1)
plusOne = arr(\x -> x+1)

plusTwo = proc a -> plusOne -< (a+1)

plusTwo2 =
    proc a -> do b <- plusOne -< a
                 plusOne -< b

plusTwo3 = plusOne >>> plusOne >>> returnA

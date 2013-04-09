-- risersOld :: (a -> a -> Ordering) ->  [a] -> [[a]]
-- risersOld cmp [] = []
-- risersOld cmp [x] = [[x]]
-- risersOld cmp (x:y:rest) =
--     case x `cmp` y of
--       GT -> [x]:(s:ss)
--       _ -> (x:s):ss
--     where (s:ss) = risersOld cmp (y:rest)

risers cmp [] = []
risers cmp [x] = [[x]]
risers cmp (x:y:rest) =
    case x `cmp` y of
      GT -> let (s:ss) = faller cmp next in (reverse (x:s)):ss
      _ -> let (s:ss) = riser cmp next in (x:s):ss
    where next = (y:rest)

riser cmp [] = []
riser cmp [x] = [[x]]
riser cmp (x:y:rest) =
    case x `cmp` y of
      GT -> let rest = risers cmp next in [x]:rest
      _ -> let (s:ss) = riser cmp next in (x:s):ss
    where next = (y:rest)

faller cmp [] = []
faller cmp [x] = [[x]]
faller cmp (x:y:rest) =
    case x `cmp` y of
      GT -> let (s:ss) = faller cmp next in (x:s):ss
      _ -> let (s:ss) = risers cmp next in []:((x:s):ss)
    where next = (y:rest)

merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge cmp [] ys = ys
merge cmp xs [] = xs
merge cmp (x:xs) (y:ys) =
    case x `cmp` y of
      GT -> y : merge cmp (x:xs) ys
      _ -> x : merge cmp xs (y:ys)

mergePairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
mergePairs cmp [] = []
mergePairs cmp [xs] = [xs]
mergePairs cmp (xs:ys:rest) = merge cmp xs ys : mergePairs cmp rest

mergesort' :: (a -> a -> Ordering) -> [[a]] -> [a]
mergesort' cmp [] = []
mergesort' cmp [xs] = xs
mergesort' cmp xss = mergesort' cmp $ mergePairs cmp xss

--old
--mergesort :: (a -> a -> Ordering) -> [a] -> [a]
--mergesort cmp = mergesort' cmp . map (:[])

--new
mergesort :: (a -> a -> Ordering) -> [a] -> [a]
mergesort cmp = mergesort' cmp . risers cmp

candidate = [1, 2, 3, 4, 70, 42, 33, 21, 12, 5, 17, 21, 13, 11, 8]

rtest = risers compare candidate
stest = mergesort compare candidate
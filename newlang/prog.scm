init [1 2 3 4]
last [1 2 3 4]

[1 2] ++ [3 4]

let (works = "let works")
    addThx msg = msg + ", thanks!"
    print (addThx works)

foo = a b c -> a + b * c

foo 3.14 5 6.1 ; 33.64 with some floating point error

factorial n =
  if n < 2
    1
    n * (factorial n-1)

factorial 6

factIter n acc =
  if n < 2
    acc
    factIter n - 1 acc * n

factIter 7 1 ;5040

;; (strictDef fib)
;;   strictProc n
;;     if n < 2
;;       n
;;       (fib n-1) + (fib n-2)

fib n = if n < 2
  n
  (fib n-1) + (fib n-2)

fib 13

identity x = x ; x->x is actually more concise than the word 'identity'

identity 52

;; boo = 1:3:8:()
;; bee = 6:42:9:()
boo = [1 3 8]
bee = [6 42 9]

zipWith (+) [boo bee]

zip [boo bee]
zip (zip [boo bee])
zip (zip (zip [boo bee]))

fibList = 0:1:(zipWith (+) fibList:(tail fibList):())
take 18 fibList

map fib (range 0 14 1)

takeWhile (6 >) (enum 0 1)

indiv p = filter (x -> x % p != 0)

sieve xs = if xs == () ()
  (p -> p : (sieve (indiv p (tail xs)))) (head xs)

primes = sieve (enum 2 1)

take 30 primes

someVar = 345 + 3
someVar
someVar := 678 + 2
someVar
someVar := print "I'm lazy!"

wee a b c = a + b * c
wee 2 7 20

(flip (-)) 18 3 ; -15

defmac (list xs) [quote xs]

list 5 7 9 11

(x -> x ** 2) 5

[[1 2] 1+3 "yay"]

print (length [1 2 3 4]) == 4

f a b = do
  print a
  print "plus"
  print b
  print "equals"
  print a + b

f 1 2

someVar ; finally prints here

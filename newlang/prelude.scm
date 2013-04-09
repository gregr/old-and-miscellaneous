(strictDef defmac)
  macro (sig body _)
    [[strictDef (head sig)] [macro (tail sig) body]]

;    (strictDef : (head sig) : ()) : ((macro : (tail sig) : body : ()) : ())

;; (strictDef defmac) ; comma version
;;   macro (sig body)
;;     [[strictDef, head sig]
;;      [macro, tail sig, body]]

;; (strictDef isNull)
;;   strictProc x
;;     (is x ())

(strictDef foldr)
  strictProc f
    strictProc acc
      strictProc xs
        if (is xs ())
          acc
          f (head xs) (foldr f acc (tail xs))

(strictDef defHelper)
  strictProc arg
    strictProc rhs
      if (isPair arg)
        if (is (head arg) (quote ($)))
          [strictProc:(head (tail arg)):rhs]
          print "ERROR: invalid parameter annotation"
;          [lazyProc:(head (tail arg)):rhs]
        [lazyProc:arg:rhs]

(strictDef toArgList)
  strictProc x
    if (isPair x)
      if (is (head x) (quote ($)))
        [x]
        x
      [x]

(strictDef defLambda)
  strictProc sig
    strictProc body
      [(foldr defHelper [body] (toArgList sig))]

defmac ((->) sig body _)
  defLambda sig body

(strictDef defParts)
  strictProc sig
    strictProc body
      if (isPair sig)
        if (is (head sig) (quote ($)))
          if (is (tail (tail sig)) ())
            [strictDef (head (tail sig)) body]
            print "ERROR: invalid definition annotation"
          strictDef : (head sig) :
            defLambda (tail sig) body
        [lazyDef sig body]

defmac ((=) sig body _)
  (parts -> [[(head parts) (head (tail parts))] (head (tail (tail parts)))])
    defParts sig body
;;   if (isPair sig)
;;     [strictDef (head sig)] :
;;       defLambda (tail sig) body
;;     [[lazyDef sig] body]

;; defmac (($=) name body _)
;;   [[strictDef name] body]

defmac ((:=) name body _)
  if (isPair name)
    if (is (head name) (quote ($)))
      [[strictSet (head (tail name))] body]
      print "ERROR: invalid assignment annotation"
    [[lazySet name] body]

;; defmac (($:=) name body _)
;;   [[strictSet name] body]

flip f a b = f b a

not x = if x false true

and x y =
  if x
    if y true false
    false

or x y =
  if x true
    if y true
      false

(!) = not
(~) = not
(&&) = and
(||) = or
(===) = is
(==) = eq
(!=) = neq
(<>) = neq
(>) = gt
(>=) = gte
(<) = lt
(<=) = lte
(+) = add
(-) = sub
(*) = mul
(/) = div
(%) = mod
(**) = pow

init xs = if (tail xs) === () ()
  (head xs) : (init (tail xs))

last xs = if (tail xs) === () (head xs)
  last (tail xs)

(@) f g x = f (g x) ; compose

seq $a b = b

doSyntax stmts =
  foldr (h t -> [(quote seq) h t]) (last stmts) (init stmts)

defmac (do stmts)
  doSyntax stmts

append xs ys = foldr (:) ys xs

(++) = append

;uncurry f xs = f (head xs) (tail ab)

;; (strictDef map)
;;   strictProc f
;;     strictProc xs
;;       if xs === ()
;;         ()
;;         pair
;;           f (head xs)
;;           map f (tail xs)

map f xs = if xs === () ()
  (f (head xs)) : (map f (tail xs))

foreach f xs = if xs === () ()
  seq (f (head xs)) (foreach f (tail xs))

defmac (let stmts)
  [[(->) [(quote _)] (doSyntax stmts)] ()]

defmac (where body stmts)
  [[(->) [(quote _)] (doSyntax (stmts ++ [body]))] ()]

;; defmac (let expr)
;; ;;   print (map (sigbody -> defParts (head (tail sigbody))
;; ;;                            (head (tail (tail sigbody))))
;; ;; 	     (init expr))
;; ; print
;;   (bindings body -> append [[(->) (map (head @ tail) bindings) body]]
;; 	                   (map (head @ tail @ tail) bindings))
;;     (map (sigbody -> defParts (head (tail sigbody))
;;                               (head (tail (tail sigbody))))
;; 	 (init expr))
;;     last expr

ap f x = f x

;; (strictDef foldl)
;;   strictProc f
;;     strictProc acc
;;       strictProc xs
;;         if xs === ()
;;           acc
;;           foldl f (f acc (head xs)) (tail xs)

foldl f acc xs = if xs === () acc
  foldl f (f acc (head xs)) (tail xs)

;; (strictDef zipWith)
;;   strictProc f
;;     strictProc xss
;;       if (head xss) === ()
;;         ()
;;         (foldl ap f (map head xss)) : (zipWith f (map tail xss))

zipWithFold fold f acc xss = if (head xss) === () ()
  (fold f acc (map head xss)) : (zipWithFold fold f acc (map tail xss))

zipWith f xss = zipWithFold foldl ap f xss

zip = zipWithFold foldr (:) ()

;; (strictDef enum)
;;   lazyProc n
;;     lazyProc i
;;       pair n
;;         enum n+i i

enum n i = n : (enum n+i i)

;; (strictDef take)
;;   strictProc n
;;     strictProc xs
;;       if n <= 0 ()
;;         pair (head xs)
;;           take n-1 (tail xs)

take n xs = if n <= 0 ()
  (head xs) :
    take n-1 (tail xs)

;; (strictDef range)
;;   strictProc start
;;     strictProc end
;;       strictProc interval
;;         take (end-start)/interval (enum start interval)

range start end interval = take (end-start)/interval (enum start interval)

length xs = where
  iter xs 0
  ;where
  iter xs l = if xs === () l (iter (tail xs) l+1)

enumerate xs = zip [(range 0 (length xs) 1) xs]

filter pred xs = if xs === () ()
  (x rest -> if (pred x) x:rest rest)
    head xs
    filter pred (tail xs)

takeWhile pred xs = if xs === () ()
  (x -> if (pred x) x:(takeWhile pred (tail xs)) ())
    head xs

repeat x = x:(repeat x)

(define (curry f x)
  (lambda args (apply f (cons x args))))

(define (y f)
  (define (h x)
    (lambda args (apply f (cons ((curry x x)) args))))
  (h h))

(define (fact f n)
  (if (< n 2) 1
      (* n (f (- n 1)))))

((y fact) 7)
;Value: 5040


(define (empty? xs) (null? xs))
(define (rest xs) (cdr xs))
(define empty '())

(define (foldl op init xs)
  (if (empty? xs)
      init
      (foldl op (op init (first xs)) (rest xs))))

(foldl + 1 (list 2 3 4))

(foldl cons empty (list 1 2 3 4))

(define (reverse-append xs lst)
  (foldl (lambda (a b) (cons b a)) lst xs))

(reverse-append (list 3 2 1) (list 4 5 6))

(define (reverse xs)
  (reverse-append xs empty))

(reverse (list 1 2 3))

(define (insertion-sort xs cmp)
  (define (insert sorted x)
    (define (inner-insert passed pending x)
      (define (aggregate) (reverse-append passed (cons x pending)))
      (if (empty? pending) (aggregate)
	  (let ((next (first pending)))
	    (if (cmp x next) (aggregate)
		(inner-insert (cons next passed) (rest pending) x)))))
    (inner-insert empty sorted x))
  (foldl (lambda (sorted next) (insert sorted next)) empty xs))
; or, this is like the foldl above (for understanding)
;  (define (sort-remaining sorted remaining)
;    (if (empty? remaining)
;	sorted
;	(sort-remaining (insert sorted (first remaining)) (rest remaining))))
;  (sort-remaining empty xs))
; brevity is one reason why foldl is awesome

(insertion-sort (list 3 65 5 76 456 1 345 2) <)
;Value 17: (1 2 3 5 65 76 345 456)

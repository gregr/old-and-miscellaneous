# Copyright (C) 2007 Gregory L. Rosenblatt
# All rights reserved.

# greg.uriel@gmail.com
# http://code.google.com/p/uriel/

# This file is part of Uriel.

# Uriel is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# Uriel is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this library.  If not, see <http://www.gnu.org/licenses/>

# common utilities: a work in progress that will probably become the prelude

# abbreviations are much easier to type
(define sfn strict-fn)
(define lfn lazy-fn)
(define fn sfn) # strict is probably going to be the default, right?

# most data constructors should probably be lazy
(define list (lfn elements elements))

(define def-compound
  (strict-fn (type)
    (macro (name params body)
      (list 'define name (list type params body)))))
      # with backquote, it would look more like this
      #`(define ,name (,type ,params ,body))))

(define def-macro (def-compound macro))
(define def-smacro (def-compound strict-macro))
(define def-sfn (def-compound sfn))
(define def-lfn (def-compound lfn))
(define def-fn (def-compound fn))

# i haven't added procs to the core environment yet... oops
#(define def-proc (def-compound proc))

# lazy or strict? i might lose my mind making this decision for every utility

(def-fn null? (x)
  (eq? x '()))

(def-fn not (x)
  (if x
      false
      true))

(define first head)
(def-fn second (p) (head (tail p)))
(def-fn third (p) (head (tail (tail p))))
(def-fn last (params)
  (if (null? (tail params))
      (head params)
      (last (tail params))))

# strict-macros are sort of a trick for performing double-evaluation
# in the calling environment
(def-smacro eval-here (expr)
  expr)

(def-fn foldl (op init seq)
  (if (null? seq)
      init
      (foldl op (op init (head seq)) (tail seq))))

(def-fn foldr (op init seq)
  (if (null? seq)
      init
      (op (head seq)
	  (foldr op init (tail seq)))))

(def-fn map-one (f seq)
  (foldr (lfn (a b) (pair (f a) b)) '() seq))

(def-smacro apply (func args)
  (pair func (map-one (fn (x) (list quote x)) args)))

(def-smacro apply-macro (func args)
  (pair func args))

(def-fn compose (a b)
  (lfn args
       (apply a (apply b args))))

(def-fn foldx-n (foldx op init seqs)
  (if (null? (head seqs))
      '()
      (pair (foldx op init (map-one head seqs))
	    (foldx-n foldx op init (map-one tail seqs)))))

(def-fn foldl-n (op init seqs) (foldx-n foldl op init seqs))
(def-fn foldr-n (op init seqs) (foldx-n foldr op init seqs))

(def-fn zip-list (seqs)
  (foldr-n pair '() seqs))

(def-fn zip seqs (zip-list seqs))

(def-fn append (a b)
  (foldr pair b a))

(def-fn inc (x)
  (+ x 1))

(def-fn dec (x)
  (- x 1))

(def-fn length (seq)
  (foldl
   (fn (result yourmom)
	(inc result))
   0 seq))

(def-fn reverse (seq)
  (foldl (fn (a b) (pair b a)) '() seq))

(def-fn map-list (f seqs)
  (foldr (lfn (a b) (pair (apply f a) b)) '() (zip-list seqs)))

(def-fn map (f . seqs)
  (map-list f seqs))

(def-macro let-list (params body)
  (pair (list apply-macro fn (list quote (list (map-one head params) body)))
	(map-one second params)))

(def-fn split-last (params)
  (letrec-list ((iter (fn (result params)
			  (if (null? (tail params))
			      (list (reverse result) (head params))
			      (iter (pair (head params) result)
				    (tail params))))))
	       (iter '() params)))

(def-macro let params
  (list apply-macro let-list (list quote (split-last params))))

(def-macro letrec params
  (list apply-macro letrec-list (list quote (split-last params))))

(def-fn filter (pred seq)
  (if (null? seq)
      '()
      (let (item (head seq))
	(if (pred item)
	    (pair item (tail seq))
	    (filter pred (tail seq))))))

# label? (named let for loops), flatten, flatmap...


(def-macro square (x)
  (list * x x))
(map square (list 1 2 3 4)) # macros can be used as first class objects! yay!

#(reverse (append (list 1 2) (list 3 4)))

(def-fn sum nums (foldl + 0 nums))

(map sum (list 1 2 3) (list 2 3 4))

(filter (fn (x) (> x 4))
	(list 1 2 3 4 5 6 7 8))

(def-fn factorial (n)
  (letrec (iter (fn (result n)
		    (if (< n 2)
			result
			(iter (* n result) (dec n)))))
    (iter 1 n)))

(factorial 7)

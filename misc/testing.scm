# Copyright (C) 2007 Gregory L. Rosenblatt
# All rights reserved

# <greg.uriel@gmail.com>
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

# non-tail-recursive factorial
(define factorial (letrec ((factorial
			    (fn (n)
				(if (< n 1)
				    1
				    (* n (factorial (- n 1)))))))
		    factorial))

# this one should support tail recursion
(define factorial-iter
  (letrec ((factorial-iter (fn (n result)
			       (if (< n 2)
				   result
				   (factorial-iter (- n 1)
						   (* result n))))))
    factorial-iter))

#(+ 4 3)

#(factorial-iter 7 1)

#(pair 1 (pair 2 nil))

# hooray for infinite lists (hooray for laziness)
(define integers (letrec
		     ((ints-iter (fn (n) (pair n (ints-iter (+ n 1))))))
                   ints-iter))

#(head (tail (tail (tail (integers 1)))))

# using the syntax-construction mechanism and var-arg function parameters to
# their fullest extent, this function becomes trivial to implement
# yay
(define list (fn args args))

(list 1 2 3 (quote apple) 4 5 (quote (banana grape)) 6)

#(. 1)
#(. 1 2 3)
#(1 . 2 3)
#(1 .. 2 3)
#(1 . 2 . 3)
(quote ((1 . 2) (1 2 . 3)))

# don't want to define append to use for list-splicing in the var-args
# example, so use this function internally to deal with passing the
# rest parameter explicitly as a list
# (assume rest is always a list for simpler error checking)
(define sum-iter
  (fn (result rest)
      (if (eq? rest nil)
	  result
	  (sum-iter (+ result (head rest)) (tail rest)))))

# simple test for var-args
(define sum (fn (first . rest)
		(sum-iter first rest)))

(sum 1 2 3 4 5)

(define identity (macro (a) (list quote a)))

(identity (hello macro world))

(define reverse-iter
  (fn (lst result)
      (if (eq? lst nil)
	  result
	  (reverse-iter (tail lst) (pair (head lst) result)))))

(define backwards (macro args
  (reverse-iter args nil)))

(backwards 1 2 +) #3

#(sum "hello" " " "world")

#(list (quote one) "two" 3)

(quote (1 (2 3) 4 . 5))

(quote (hello\(world\)))

#"hello world"
"hello \"world\""

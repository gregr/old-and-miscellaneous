; comments follow semi-colons until the end of the line, like so

syntax is comprised of lisp-like s-expressions that can be explicitly formed (within parens) or inferred based on layout (indentation)

for example, the following two expressions are parsed the same way:

; 1st
foo a b
  f c d
    g e
  h i

; 2nd
(foo a b (f c d (g e)) (h i))

function application is specified via adjacency and all functions are curried:

g a b ; applies g to a, and then applies the result to b

;;;; definitions

x = 23 ; defines a variable x

f a b = a + b ;defines a function f that takes two arguments and adds them

a b -> a + b ;creates an anonymous function of two arguments

f = a b -> a + b ;this is equivalent to the original definition of f

by default, function argument evaluation is lazy

h $a b = b ;$ is a strictness annotation for the argument 'a'

h (print "hello") 4 ;will print "hello" due to being strict in the first arg

;;;; lists

() ;this is the null value, and can also be thought of as the empty list

null-terminated lists can be constructed using the lazy pairing operator:

a : b : c : () ;the ':' operator is right-associative

a : (b : (c : ())) ;so it's parsed like this

[a b c] ;this syntax is a more convenient way to express the same thing

because lists are constructed from lazy pairs, infinite data structures can be represented

x = 1:x ;defines an infinite list of 1's

take 20 x ;but you should only evaluate what you need

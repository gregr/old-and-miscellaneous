(add-tests-with-string-output "procedures"
  [(letrec () 12) => "12\n"]
  [(letrec () (let ([x 5]) (fx+ x x))) => "10\n"]
  [(letrec ([f (lambda () 5)]) 7) => "7\n"]
  [(letrec ([f (lambda () 5)]) (let ([x 12]) x)) => "12\n"]
  [(letrec ([f (lambda () 5)]) (f)) => "5\n"]
  [(letrec ([f (lambda () 5)]) (let ([x (f)]) x)) => "5\n"]
  [(letrec ([f (lambda () 5)]) (fx+ (f) 6)) => "11\n"]
  [(letrec ([f (lambda () 5)]) (fx- 20 (f))) => "15\n"]
  [(letrec ([f (lambda () 5)]) (fx+ (f) (f))) => "10\n"]
  [(letrec ([f (lambda () (fx+ 5 7))]
            [g (lambda () 13)]) 
    (fx+ (f) (g))) => "25\n"]
  [(letrec ([f (lambda (x) (fx+ x 12))]) (f 13)) => "25\n"]
  [(letrec ([f (lambda (x) (fx+ x 12))]) (f (f 10))) => "34\n"]
  [(letrec ([f (lambda (x) (fx+ x 12))]) (f (f (f 0)))) => "36\n"]
  [(letrec ([f (lambda (x y) (fx+ x y))] 
            [g (lambda (x) (fx+ x 12))])
    (f 16 (f (g 0) (fx+ 1 (g 0))))) => "41\n"]
  [(letrec ([f (lambda (x) (g x x))]
            [g (lambda (x y) (fx+ x y))])
     (f 12)) => "24\n"]
  [(letrec ([f (lambda (x) 
                 (if (fxzero? x)
                     1
                     (fx* x (f (fxsub1 x)))))])
      (f 5)) => "120\n"]
  [(letrec ([e (lambda (x) (if (fxzero? x) #t (o (fxsub1 x))))]
            [o (lambda (x) (if (fxzero? x) #f (e (fxsub1 x))))])
     (e 25)) => "#f\n"]
)

(add-tests-with-string-output "deeply nested procedures"
  [(letrec ([sum (lambda (n ac)
                   (if (fxzero? n)
                        ac
                        (sum (fxsub1 n) (fx+ n ac))))])
    (sum 10000 0)) => "50005000\n"]
  [(letrec ([e (lambda (x) (if (fxzero? x) #t (o (fxsub1 x))))]
            [o (lambda (x) (if (fxzero? x) #f (e (fxsub1 x))))])
     (e 5000000)) => "#t\n"]
)

(add-tests-with-string-output "binary primitives"

  [(fxlognot -7) => "6\n"]
  [(fxlognot (fxlogor (fxlognot 7) 1)) => "6\n"]
  [(fxlognot (fxlogor (fxlognot 7) (fxlognot 2))) => "2\n"]
  [(fxlogand (fxlognot (fxlognot 12)) (fxlognot (fxlognot 12))) => "12\n"]
  [(fx+ (fx+ 1 2) (fx+ 3 4)) => "10\n"]
  [(fx+ (fx+ 1 2) (fx+ 3 -4)) => "2\n"]
  [(fx+ (fx+ 1 2) (fx+ -3 4)) => "4\n"]
  [(fx+ (fx+ 1 2) (fx+ -3 -4)) => "-4\n"]
  [(fx+ (fx+ 1 -2) (fx+ 3 4)) => "6\n"]
  [(fx+ (fx+ 1 -2) (fx+ 3 -4)) => "-2\n"]
  [(fx+ (fx+ 1 -2) (fx+ -3 4)) => "0\n"]
  [(fx+ (fx+ 1 -2) (fx+ -3 -4)) => "-8\n"]
  [(fx+ (fx+ -1 2) (fx+ 3 4)) => "8\n"]
  [(fx+ (fx+ -1 2) (fx+ 3 -4)) => "0\n"]
  [(fx+ (fx+ -1 2) (fx+ -3 4)) => "2\n"]
  [(fx+ (fx+ -1 2) (fx+ -3 -4)) => "-6\n"]
  [(fx+ (fx+ -1 -2) (fx+ 3 4)) => "4\n"]
  [(fx+ (fx+ -1 -2) (fx+ 3 -4)) => "-4\n"]
  [(fx+ (fx+ -1 -2) (fx+ -3 4)) => "-2\n"]
  [(fx+ (fx+ -1 -2) (fx+ -3 -4)) => "-10\n"]
  [(fx+ (fx+ (fx+ (fx+ (fx+ (fx+ (fx+ (fx+ 1 2) 3) 4) 5) 6) 7) 8) 9) => "45\n"]
  [(fx+ 1 (fx+ 2 (fx+ 3 (fx+ 4 (fx+ 5 (fx+ 6 (fx+ 7 (fx+ 8 9)))))))) => "45\n"]
  [(fx+ (fx+ (fx+ (fx+ 1 2) (fx+ 3 4)) (fx+ (fx+ 5 6) (fx+ 7 8)))
        (fx+ (fx+ (fx+ 9 10) (fx+ 11 12)) (fx+ (fx+ 13 14) (fx+ 15 16)))) 
   => "136\n"]
  [(fx- (fx- 1 2) (fx- 3 4)) => "0\n"]
  [(fx- (fx- 1 2) (fx- 3 -4)) => "-8\n"]
  [(fx- (fx- 1 2) (fx- -3 4)) => "6\n"]
  [(fx- (fx- 1 2) (fx- -3 -4)) => "-2\n"]
  [(fx- (fx- 1 -2) (fx- 3 4)) => "4\n"]
  [(fx- (fx- 1 -2) (fx- 3 -4)) => "-4\n"]
  [(fx- (fx- 1 -2) (fx- -3 4)) => "10\n"]
  [(fx- (fx- 1 -2) (fx- -3 -4)) => "2\n"]
  [(fx- (fx- -1 2) (fx- 3 4)) => "-2\n"]
  [(fx- (fx- -1 2) (fx- 3 -4)) => "-10\n"]
  [(fx- (fx- -1 2) (fx- -3 4)) => "4\n"]
  [(fx- (fx- -1 2) (fx- -3 -4)) => "-4\n"]
  [(fx- (fx- -1 -2) (fx- 3 4)) => "2\n"]
  [(fx- (fx- -1 -2) (fx- 3 -4)) => "-6\n"]
  [(fx- (fx- -1 -2) (fx- -3 4)) => "8\n"]
  [(fx- (fx- -1 -2) (fx- -3 -4)) => "0\n"]
  [(fx- (fx- (fx- (fx- (fx- (fx- (fx- (fx- 1 2) 3) 4) 5) 6) 7) 8) 9) => "-43\n"]
  [(fx- 1 (fx- 2 (fx- 3 (fx- 4 (fx- 5 (fx- 6 (fx- 7 (fx- 8 9)))))))) => "5\n"]
  [(fx- (fx- (fx- (fx- 1 2) (fx- 3 4)) (fx- (fx- 5 6) (fx- 7 8)))
        (fx- (fx- (fx- 9 10) (fx- 11 12)) (fx- (fx- 13 14) (fx- 15 16)))) 
   => "0\n"]
  [(fx* (fx* (fx* (fx* 2 3) (fx* 4 5)) (fx* (fx* 6 7) (fx* 8 9)))
        (fx* (fx* (fx* 2 3) (fx* 2 3)) (fx* (fx* 2 3) (fx* 2 3)))) 
   => "470292480\n"]
  [(fxlognot (fxlogor (fxlognot 7) 1)) => "6\n"]
  [(fxlognot (fxlogor (fxlognot 7) (fxlognot 2))) => "2\n"]
  [(fxlogand (fxlognot (fxlognot 12)) (fxlognot (fxlognot 12))) => "12\n"]
  [(fx= (fx+ 13 3) (fx+ 10 6)) => "#t\n"]
  [(fx= (fx+ 13 0) (fx+ 10 6)) => "#f\n"]
  [(fx= (fx+ 12 1) (fx+ -12 -1)) => "#f\n"]
  [(fx< (fx+ 10 6) (fx+ 13 1)) => "#f\n"]
  [(fx< (fx+ 10 6) (fx+ 13 3)) => "#f\n"]
  [(fx< (fx+ 10 6) (fx+ 13 31)) => "#t\n"]
  [(fx< (fx+ 12 1) (fx+ -12 -1)) => "#f\n"]
  [(fx< (fx+ -12 -1) (fx+ 12 1)) => "#t\n"]
  [(fx<= (fx+ 10 6) (fx+ 13 1)) => "#f\n"]
  [(fx<= (fx+ 10 6) (fx+ 13 3)) => "#t\n"]
  [(fx<= (fx+ 10 6) (fx+ 13 31)) => "#t\n"]
  [(fx<= (fx+ 12 1) (fx+ -12 -1)) => "#f\n"]
  [(fx<= (fx+ -12 -1) (fx+ 12 1)) => "#t\n"]
  [(fx> (fx+ 10 6) (fx+ 13 1)) => "#t\n"]
  [(fx> (fx+ 10 6) (fx+ 13 3)) => "#f\n"]
  [(fx> (fx+ 10 6) (fx+ 13 31)) => "#f\n"]
  [(fx> (fx+ 12 1) (fx+ -12 -1)) => "#t\n"]
  [(fx> (fx+ -12 -1) (fx+ 12 1)) => "#f\n"]
  [(fx>= (fx+ 10 6) (fx+ 13 1)) => "#t\n"]
  [(fx>= (fx+ 10 6) (fx+ 13 3)) => "#t\n"]
  [(fx>= (fx+ 10 6) (fx+ 13 31)) => "#f\n"]
  [(fx>= (fx+ 12 1) (fx+ -12 -1)) => "#t\n"]
  [(fx>= (fx+ -12 -1) (fx+ 12 1)) => "#f\n"]
)


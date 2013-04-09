(define eval-scheme eval)
(define apply-scheme apply)

(define pair cons)
(define head car)
(define tail cdr)

(define (curry f arg)
  (lambda args (apply-scheme f (pair arg args))))
(define (compose f g)
  (lambda args (f (apply-scheme g args))))
;(define (pair a b) (lambda (f) (f a b)))
;(define (head p) (p (lambda (a b) a)))
;(define (tail p) (p (lambda (a b) b)))

(define (make-tag type rep) (pair type rep))
(define (tag-type tag) (head tag))
(define (tag-rep tag) (tail tag))
(define (tag? obj) (pair? obj))
(define (tagged? tag obj) (if (tag? obj) (eq? tag (tag-type obj)) false))

(define (lazy-value env obj) (make-thunk obj env))
(define (strict-value env obj) (actual-value (execute env obj)))
(define (actual-value value)
  (if (thunk? value)
      (eval-thunk value)
      value))
(define (lazy-values env objs) (map (curry lazy-value env) objs))
(define (strict-values env objs) (map (curry strict-value env) objs))
(define (syntax-values env objs) objs)

(define (thunk? obj) (tagged? 'thunk obj))
(define (make-thunk value env) (make-tag 'thunk (pair value env)))
(define (thunk-value t) (head t))
(define (thunk-env t) (tail t))
(define (evaluated-thunk? t) (null? (thunk-env t)))
(define (update-thunk t value) (set-car! t value) (set-cdr! t '())) ;effects...
(define (eval-thunk obj)
  (let ((t (tag-rep obj)))
    (if (evaluated-thunk? t)
	(thunk-value t)
	(let ((result (strict-value (thunk-env t) (thunk-value t))))
	  (update-thunk t result)
	  result))))

(define (make-frame vars vals) (pair vars vals))
(define (frame-variables frame) (head frame))
(define (frame-values frame) (tail frame))
(define (frame-bind! frame var val)
  (set-car! frame (pair var (frame-variables frame)))
  (set-cdr! frame (pair val (frame-values frame))))
(define (frame-define! frame var val)
  (define (scan vars vals)
    (if (null? vars) (frame-bind! frame var val)
	(if (eq? var (head vars))
	    (set-car! vals val)
	    (scan (tail vars) (tail vals)))))
  (scan (frame-variables frame) (frame-values frame)))

(define root-environment '(root-environment))
(define (environment-frame env) (head env))
(define (environment-parent env) (tail env))

(define (extend-environment env vars vals)
  (let ((vars-length (length vars)) (vals-length (length vals)))
    (if (= vars-length vals-length)
	(pair (make-frame vars vals) env)
	(if (< vars-length vals-length)
	    (error "too many arguments supplied:" vars vals)
	    (error "too few arguments supplied:" vars vals)))))

(define (parse-extend-environment env vars vals) ;supports dotted-list varargs
  (define (parse vars vals result-vars result-vals)
    (if (null? vars)
	(extend-environment env result-vars result-vals)
	(if (pair? vars)
	    (parse (tail vars) (tail vals)
		   (pair (head vars) result-vars)
		   (pair (head vals) result-vals))
	    (extend-environment env (pair vars result-vars)
				(pair vals result-vals)))))
  (parse vars vals '() '()))

(define (environment-lookup-checked env var)
  (define (iter env)
    (define (scan vars vals)
      (if (null? vars)
	  (iter (environment-parent env))
	  (if (eq? var (head vars))
	      (list (head vals))
	      (scan (tail vars) (tail vals)))))
    (if (eq? env root-environment)
	'() ;if not found
	(let ((frame (environment-frame env)))
	  (scan (frame-variables frame) (frame-values frame)))))
  (iter env))

(define (unassigned x) unassigned)

(define (environment-lookup env var)
  (let ((result (environment-lookup-checked env var)))
    (if (null? result)
	(error "unbound variable:" var)
	(if (eq? unassigned result)
	    (error "premature reference to variable:" var)
	    (first result)))))

(define (environment-define! env var val)
  (frame-define! (environment-frame env) var val))

(define (unit) unit) ;another idea for the following types, inspired by haskell
(define macro-type '(macro)) ; must guarantee uniqueness
(define syntax-type '(syntax))
(define procedure-type '(procedure))
(define (macro? obj) (tagged? macro-type obj))
(define (make-macro proc) (make-tag macro-type proc))
(define (syntax? obj) (tagged? syntax-type obj))
(define (make-syntax proc) (make-tag syntax-type proc))
(define (procedure? proc) (tagged? procedure-type proc))
(define (make-procedure proc) proc); (make-tag procedure-type proc))

(define (variable? expr) (symbol? expr))
(define (application? expr) (pair? expr)) ;assume non-dotted list
(define (application-operator app) (head app))
(define (application-operands app) (tail app))

;TODO: pass compile-time env during analysis for macro-expansion
(define (analyze-atom a) (lambda (env) a))
(define (analyze-variable v) (lambda (env) (environment-lookup env v)))
(define (analyze-procedure-application env op args)
  (let ((op (analyze env op)) (args (analyze-sequence env args)))
    (lambda (env) (apply env op args))))
(define (analyze-macro-application env macro args)
  (analyze env (apply-expansion env macro args)))
(define (analyze-syntax-application env syntax args)
  (apply-expansion env syntax args)) ;quote the result
(define (analyze-application env app)
  (let ((op (application-operator app)) (args (application-operands app)))
    (if (variable? op)
	(let ((op-value (environment-lookup-checked env op)))
	  (if (null? op-value)
	      (analyze-procedure-application env op args)
	      (let ((op-value (first op-value)))
		(if (macro? op-value)
		    (analyze-macro-application env op-value args)
		    (if (syntax? op-value)
			(analyze-syntax-application env op-value args)
			(analyze-procedure-application env op args))))))
	(analyze-procedure-application env op args))))
(define (analyze env expr)
  (if (variable? expr) (analyze-variable expr)
      (if (application? expr) (analyze-application env expr)
	  (analyze-atom expr))))
(define (analyze-sequence env operands)
  (map (lambda (operand) (analyze env operand)) operands))

(define (eval env expr) (strict-value env (analyze env expr)))
(define (apply-expansion env expander args) ((tag-rep expander) env args))
(define (apply env op args) ((strict-value env op) env args))
(define (execute env obj) (obj env)) ;simple version

(define (no-op env) '())

(define input-prompt "lazy>> ")
(define output-prompt ";value: ")
(define (prompt-for-input str) (newline) (newline) (display str))
(define (announce-output str) (newline) (display str))
(define (driver-loop env)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval env input)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop env))
(define (user-print object)
  (display object))

(define (ensure-exact-args count args) (ensure-args = count args))
(define (ensure-atleast-args count args) (ensure-args <= count args))
(define (ensure-args pred count args)
  (let ((args-length (length args)))
    (if (not (pred count args-length))
	(error "incorrect number of arguments supplied: (received, expected):"
	       args-length count))))

(define (construct-primitives constructor primitives)
   (map (lambda (p) (pair (head p) (constructor (tail p)))) primitives))
(define (make-initial-environment)
  (let ((vars (map head primitive-named-values))
	(vals (map tail primitive-named-values)))
    (extend-environment root-environment vars vals)))

(define (make-primitive eval-proc count op)
  (lambda (env args)
    (ensure-exact-args count args)
    (apply-scheme op (eval-proc env args))))
(define (make-strict count op) (make-primitive strict-values count op))
(define (make-lazy count op) (make-primitive lazy-values count op))

(define (prim-def env args)
  (ensure-exact-args 2 args)
  (let ((var (first args)) (body (analyze env (second args))))
    (environment-define! env var (execute env body))
    no-op))

(define (prim-letrec env args)
  (ensure-exact-args 2 args)
  (let ((bindings (first args)) (body (analyze env (second args))))
    (let ((vars (map first bindings))
	  (bindings (map (lambda (binding)
			   (pair (first binding)
				 (analyze env (second binding)))) bindings)))
      (lambda (calling-env)
	(let ((new-env (extend-environment calling-env vars
					   (map unassigned vars))))
	  (for-each (lambda (binding)
		      (environment-define! new-env (head binding)
					   (lazy-value new-env
						       (tail binding))))
		    bindings)
	  (execute new-env body))))))
  

(define (prim-fn-template eval-proc env args)
  (ensure-exact-args 2 args)
  (let ((params (first args)) (body (analyze env (second args))))
    (lambda (creating-env)
      (lambda (calling-env args)
	(execute (parse-extend-environment creating-env params
					   (eval-proc calling-env args))
		 body)))))
(define prim-sfn (curry prim-fn-template strict-values))
(define prim-lfn (curry prim-fn-template lazy-values))
(define prim-syntax
  (compose make-syntax (curry prim-fn-template syntax-values)))
(define prim-macro
  (compose make-macro (curry prim-fn-template syntax-values)))

(define (prim-quote env args)
  (ensure-exact-args 1 args)
  (analyze-atom (first args)))

(define (prim-if condition consequent alternative)
  (if (eval-thunk condition) consequent alternative))

(define primitive-syntaxes
  (list
   (pair 'def prim-def)
   (pair 'letrec prim-letrec)
   (pair 'quote prim-quote)
   (pair 'fn prim-lfn)
   (pair 'sfn prim-sfn)
   (pair 'syntax prim-syntax)
   (pair 'macro prim-macro)))
(define primitive-macros (list))
(define primitive-procedures
  (list
   (pair 'pair (make-lazy 2 pair))
   (pair 'head (make-strict 1 head))
   (pair 'tail (make-strict 1 tail))
   (pair 'if (make-lazy 3 prim-if))
   (pair 'eq? (make-strict 2 eq?))
   (pair 'pair? (make-strict 2 pair?))
   (pair '= (make-strict 2 =))
   (pair '+ (make-strict 2 +))
   (pair '- (make-strict 2 -))
   (pair '* (make-strict 2 *))
   (pair '/ (make-strict 2 /))))

(define primitive-named-values
  (let ((syntaxes (construct-primitives make-syntax primitive-syntaxes))
	(macros (construct-primitives make-macro primitive-macros))
	(procedures (construct-primitives make-procedure primitive-procedures)))
    (append syntaxes macros procedures)))
(define initial-environment (make-initial-environment))

(define (repl) (driver-loop initial-environment))

;TODO: rewrite: define(s) -> letrec ?

;TODO: laziness allows run-time argument errors; (pair 5) in the following:
;(head (tail (tail (pair 3 (pair 4 (pair 5))))))
;I guess this kind of problem really can't be fixed without static typing

;((fn (f a b) (f a b)) + 1 2)

;y-combinator
;(fn (f) ((fn (x) (f (x x))) (fn (x) (f (x x)))))
;(fn (ones) (pair 1 (ones)))
;((fn (f) ((fn (x) (f (x x))) (fn (x) (f (x x))))) (fn (ones) (pair 1 (ones))))
;(fn (f) (f f))
;(fn (ones) (pair 1 (ones ones)))
;((fn (f) (f f)) (fn (ones) (pair 1 (ones ones))))
;(fn (integers start) (pair start (integers integers (+ 1 start))))
;((fn (f) (f f 0)) (fn (integers start) (pair start (integers integers (+ 1 start)))))

;(def test (fn (x)
;	      (letrec ((even? (fn (n) (if (= n 0) #t (odd? (- n 1)))))
;		       (odd? (fn (n) (if (= n 0) #f (even? (- n 1))))))
;		(even? x))))

;(define test (lambda (x)
;	       (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
;			(odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
;		 (even? x))))

;(test 5)

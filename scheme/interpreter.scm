(define eval-scheme eval)
(define apply-scheme apply)

; strict-value, atom?, eval-atom

(define (eval env expr)
  (let ((expr (strict-value expr)))
    (if (null? env)
	expr
	(cond ((atom? expr)
	       (eval-atom expr))
	      (else ; if it's not an atom, it should be a sequence
	       (let ((result (apply env (car expr) (cdr expr))))
		 (eval (car result) (cdr result))))))))

; primitive-proc?, compound-proc?, apply-prim, apply-compound

(define (apply env proc args)
  (let ((proc (eval env proc)))
    (cond ((primitive-procedure? proc)
	   (apply-primitive-procedure env proc args))
	  ((compound-procedure? proc)
	   (apply-compound-procedure env proc args))
	  (else
	   (error "Unknown procedure type -- apply" proc)))))

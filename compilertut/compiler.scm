(load "tests/tests-driver.scm")
(load "tests/tests-1.8-req.scm")
(load "tests/tests-1.7-req.scm")
(load "tests/tests-1.6-req.scm")
(load "tests/tests-1.5-req.scm")
(load "tests/tests-1.4-req.scm")
(load "tests/tests-1.3-req.scm")
(load "tests/tests-1.2-req.scm")
(load "tests/tests-1.1-req.scm")

(define fxshift        2)
(define fxmask      #x03)
(define fxtag       #x00)

(define bool-f      #x2F)
(define bool-t      #x6F)
(define bool-bit       6)
(define bool-mask   #xBF)
(define rep-nil     #x3F)
(define charshift      8)
(define chartag     #x0F)
(define charmask    #xFF)
(define wordsize       8) ; bytes

(define tag-mask    #x07)
(define pair-tag    #x01)
(define vector-tag  #x05)
(define string-tag  #x06)

(define fixnum-bits (- (* wordsize 8) fxshift))
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))
(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))
(define (immediate-rep x)
  (cond
   [(fixnum? x) (ash x fxshift)]
   [(boolean? x) (if x bool-t bool-f)]
   [(char? x) (+ chartag (ash (char->integer x) charshift))]
   [(null? x) rep-nil]))

(define unique-label
  (let ([count 0])
    (lambda (name)
      (let ([L (format "L_~s~s" name count)])
        (set! count (add1 count))
        L))))

(define (unique-labels names)
  (map (lambda (name) (unique-label name)) names))

(define-syntax define-primitive
  (syntax-rules ()
   [(_ (prim-name si env arg* ...) b b* ...)
    (begin
      (putprop 'prim-name '*is-prim* #t)
      (putprop 'prim-name '*arg-count*
        (length '(arg* ...)))
      (putprop 'prim-name '*emitter*
        (lambda (si env arg* ...) b b* ...)))]))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))
(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error 'primitive-emitter "invalid primitive" x)))
(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))
(define (check-primcall-args prim args)
  (let ([argc (getprop prim '*arg-count*)] [argsl (length args)])
    (or (eq? argc argsl)
        (error 'check-primcall-args "primcall ~s takes ~s args; received ~s"
               prim argc argsl))))

(define (tagged? tag expr) (and (pair? expr) (eq? tag (car expr))))
(define (if? expr) (tagged? 'if expr))
(define (if-test expr) (cadr expr))
(define (if-conseq expr) (caddr expr))
(define (if-altern expr) (cadddr expr))

(define (variable? expr) (symbol? expr))
(define (let? expr) (tagged? 'let expr))
(define (let-bindings expr) (cadr expr))
(define (let-body expr) (caddr expr))

(define (empty? bindings) (null? bindings))
(define (first bindings) (car bindings))
(define (rest bindings) (cdr bindings))
(define (lhs binding) (car binding))
(define (rhs binding) (cadr binding))

(define (empty-env) '())
(define (extend-env var val env) (cons (list var val) env))
(define (zip2err) (error 'zip2 "list args not of the same length"))
(define (zip2 xs ys)
  (if (null? xs) (if (null? ys) (list) (zip2err))
      (if (null? ys) (zip2err)
          (cons (list (car xs) (car ys)) (zip2 (cdr xs) (cdr ys))))))
(define (make-initial-env vars vals)
  (fold-left (lambda (env xy) (extend-env (car xy) (cadr xy) env))
             (empty-env) (zip2 vars vals)))
(define (lookup-env v env)
  (if (empty? env) #f
      (if (eq? v (caar env)) (cadar env)
          (lookup-env v (cdr env)))))

(define (lambda? expr) (tagged? 'lambda expr))
(define (lambda-formals lam) (cadr lam))
(define (lambda-body lam) (caddr lam))
(define (check-lambda expr)
  (or (lambda? expr) (error 'check-lambda "expected lambda" expr)))

(define (letrec? expr) (tagged? 'letrec expr))
(define (letrec-bindings expr) (cadr expr))
(define (letrec-body expr) (caddr expr))

;(define (app? expr) (tagged? 'app expr))
(define (app-target expr) (car expr))
(define (app-args expr) (cdr expr))

; todo: and, or
;; (define-syntax and
;;   (syntax-rules ()
;;    [(and) #t]
;;    [(and test) test]
;;    [(and test test* ...)
;;     (if test (and test* ...) #f)]))

(define (next-si si) (- si wordsize))
(define (prev-si si) (+ si wordsize))
(define (next-hi hi) (+ hi wordsize))
(define (emit-stack-save si)
  (emit "    movq %rax, ~s(%rsp)" si))
(define (emit-stack-load si)
  (emit "    movq ~s(%rsp), %rax" si))
(define (emit-heap-save hi)
  (emit "    movq %rax, ~s(%rbp)" hi))
(define (emit-adjust-heap hi)
  (emit "    addq $~s, %rbp" hi))

(define (emit-obj tag)
  (emit "    movq %rbp, %rax")
  (emit "    orq $~s, %rax" tag))
(define (emit-immediate x)
  (emit "    movq $~s, %rax" (immediate-rep x)))
(define (emit-variable-ref env var)
  (cond
   [(lookup-env var env) => emit-stack-load]
   [else (error 'emit-variable-ref "unbound variable ref" var)]))
(define (emit-if si env expr emex)
  (let ([alt-label (unique-label 'alt)]
        [end-label (unique-label 'end)])
    (emit-expr si env (if-test expr))
    (emit "    cmpq $~s, %rax" bool-f)
    (emit "    je ~a" alt-label)
    (emex si env (if-conseq expr))
    (emit "    jmp ~a" end-label)
    (emit "~a:" alt-label)
    (emex si env (if-altern expr))
    (emit "~a:" end-label)))
(define (emit-let si env expr emex)
  (define (process-let bindings si new-env)
    (cond
     [(empty? bindings) (emex si new-env (let-body expr))]
     [else
      (let ([b (first bindings)])
        (emit-expr si env (rhs b))
        (emit-stack-save si)
        (process-let (rest bindings)
           (next-si si)
           (extend-env (lhs b) si new-env)))]))
  (process-let (let-bindings expr) si env))
(define (emit-lambda env)
  (lambda (expr label)
    (emit-function-header label)
    (let ([fmls (lambda-formals expr)]
          [body (lambda-body expr)])
      (let f ([fmls fmls] [si (- wordsize)] [env env])
        (cond
         [(empty? fmls) (emit-tail-expr si env body) (emit "    ret")]
         [else
          (f (rest fmls)
             (next-si si)
             (extend-env (first fmls) si env))])))))

(define (emit-letrec expr)
  (let* ([bindings (letrec-bindings expr)]
         [lvars (map lhs bindings)]
         [lambdas (map rhs bindings)]
         [labels (unique-labels lvars)]
         [env (make-initial-env lvars labels)])
    (for-each check-lambda lambdas)
    (for-each (emit-lambda env) lambdas labels)
    (emit-scheme-entry (letrec-body expr) env)))

(define (emit-adjust-base si)
  (emit "    addq $~s, %rsp" si))
(define (emit-call si tgt)
;  (emit "    movq %rsp ~s(%rsp)" si)
  (emit "    call ~a" tgt))
(define (emit-tail-call si tgt)
;  (emit "    movq %rsp ~s(%rsp)" si)
  (emit "    jmp ~a" tgt))

(define (emit-app si env expr)
  (define (emit-arguments si args)
    (unless (empty? args)
      (emit-expr si env (first args))
      (emit-stack-save si)
      (emit-arguments (next-si si) (rest args))))
  (emit-arguments (next-si si) (app-args expr))
  (emit-adjust-base (prev-si si))
  (emit-call si (lookup-env (app-target expr) env))
  (emit-adjust-base (- (prev-si si))))
(define (emit-tail-argshift si tsi args)
  (unless (empty? args)
    (emit-stack-load si)
    (emit-stack-save tsi)
    (emit-tail-argshift (next-si si) (next-si tsi) (rest args))))
(define (emit-tail-app si env expr)
  (define (emit-arguments si args)
    (unless (empty? args)
      (emit-expr si env (first args))
      (emit-stack-save si)
      (emit-arguments (next-si si) (rest args))))
  (emit-arguments si (app-args expr))
  (emit-tail-argshift si (next-si 0) (app-args expr))
  (emit-tail-call si (lookup-env (app-target expr) env)))

(define (emit-primcall si env expr)
  (let ((prim (car expr)) (args (cdr expr)))
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)))

(define (emit-expr si env expr)
  (cond
   [(immediate? expr) (emit-immediate expr)]
   [(variable? expr)  (emit-variable-ref env expr)]
   [(if? expr)        (emit-if si env expr emit-expr)]
   [(let? expr)       (emit-let si env expr emit-expr)]
   [(primcall? expr)  (emit-primcall si env expr)]
   [else (emit-app si env expr)]))
;   [else (error 'emit-expr "invalid expr" expr)]))
(define (emit-tail-expr si env expr)
  (cond
   [(immediate? expr) (emit-immediate expr)]
   [(variable? expr) (emit-variable-ref env expr)]
   [(if? expr)        (emit-if si env expr emit-tail-expr)]
   [(let? expr)       (emit-let si env expr emit-tail-expr)]
   [(primcall? expr) (emit-primcall si env expr)]
   [else (emit-tail-app si env expr)]))

(define (emit-function-header name)
  (unless (string? name) (error 'emit-function-header
                                "function name must be a string" name))
  (emit "    .text")
  (emit "    .globl ~a" name)
  (emit "    .type ~a, @function" name)
  (emit "~a:" name))
(define (emit-scheme-entry expr env)
  (emit-function-header "L_scheme_entry")
  (emit-tail-expr (- wordsize) env expr)
  (emit "    ret"))
(define (emit-program expr)
  (if (letrec? expr) (emit-letrec expr)
      (emit-scheme-entry expr (empty-env)))
  (emit-function-header "scheme_entry")
;  (emit "    movq %rsp, %rcx")
;  (emit "    movq 32(%rsp), %rsp")
  (emit "    movq %rbp, %rcx")
  (emit "    movq 32(%rsp), %rbp")
  (emit "    call L_scheme_entry")
  (emit "    movq %rcx, %rbp")
;  (emit "    movq %rcx, %rsp")
  (emit "    ret"))

(define (emit-cmp->bool cmp)
  (emit "    set~a %al" cmp)
  (emit "    movzbq %al, %rax")
  (emit "    sal $~s, %al" bool-bit)
  (emit "    orq $~s, %rax" bool-f))

(define-primitive (fxadd1 si env arg)
  (emit-expr si env arg)
  (emit "    addq $~s, %rax" (immediate-rep 1)))
(define-primitive (fxsub1 si env arg)
  (emit-expr si env arg)
  (emit "    subq $~s, %rax" (immediate-rep 1)))
(define-primitive (fixnum->char si env arg)
  (emit-expr si env arg)
  (emit "    shlq $~s, %rax" (- charshift fxshift))
  (emit "    orq $~s, %rax" chartag))
(define-primitive (char->fixnum si env arg)
  (emit-expr si env arg)
  (emit "    shrq $~s, %rax" (- charshift fxshift)))

(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (emit "    andq $~s, %rax" fxmask)
  (emit "    cmpq $~s, %rax" fxtag)
  (emit-cmp->bool "e"))

(define-primitive (fxzero? si env arg)
  (emit-expr si env arg)
  (emit "    cmpq $0, %rax")
  (emit-cmp->bool "e"))

(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (emit "    cmpq $~s, %rax" rep-nil)
  (emit-cmp->bool "e"))

(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (emit "    andq $~s, %rax" bool-mask)
  (emit "    cmpq $~s, %rax" bool-f)
  (emit-cmp->bool "e"))

(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit "    andq $~s, %rax" charmask)
  (emit "    cmpq $~s, %rax" chartag)
  (emit-cmp->bool "e"))

(define-primitive (not si env arg)
  (emit-expr si env arg)
  (emit "    cmpq $~s, %rax" bool-f)
  (emit-cmp->bool "e"))

(define-primitive (fxlognot si env arg)
  (emit-expr si env arg)
  (emit "    xorq $~s, %rax" -4))

(define-primitive (fx+ si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-stack-save si)
  (emit-expr (next-si si) env arg2)
  (emit "    addq ~s(%rsp), %rax" si))
(define-primitive (fx- si env arg1 arg2)
  (emit-expr si env arg2)
  (emit-stack-save si)
  (emit-expr (next-si si) env arg1)
  (emit "    subq ~s(%rsp), %rax" si))
(define-primitive (fx* si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "    shrq $~s, %rax" fxshift)
  (emit-stack-save si)
  (emit-expr (next-si si) env arg2)
  (emit "    mulq ~s(%rsp)" si))
(define-primitive (fxlogor si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-stack-save si)
  (emit-expr (next-si si) env arg2)
  (emit "    orq ~s(%rsp), %rax" si))
(define-primitive (fxlogand si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-stack-save si)
  (emit-expr (next-si si) env arg2)
  (emit "    andq ~s(%rsp), %rax" si))
(define (emit-fxcmp si env arg1 arg2 cmp)
  (emit-expr si env arg2)
  (emit-stack-save si)
  (emit-expr (next-si si) env arg1)
  (emit "    cmpq ~s(%rsp), %rax" si)
  (emit-cmp->bool cmp))
(define-primitive (fx= si env arg1 arg2) (emit-fxcmp si env arg1 arg2 "e"))
(define-primitive (fx< si env arg1 arg2) (emit-fxcmp si env arg1 arg2 "l"))
(define-primitive (fx<= si env arg1 arg2) (emit-fxcmp si env arg1 arg2 "le"))
(define-primitive (fx> si env arg1 arg2) (emit-fxcmp si env arg1 arg2 "g"))
(define-primitive (fx>= si env arg1 arg2) (emit-fxcmp si env arg1 arg2 "ge"))

(define-primitive (pair? si env arg)
  (emit-expr si env arg)
  (emit "    andq $~s, %rax" tag-mask)
  (emit "    cmpq $~s, %rax" pair-tag)
  (emit-cmp->bool "e"))

(define-primitive (cons si env arg1 arg2)
  (emit-expr si env arg2)
  (emit-stack-save si)
  (emit-expr (next-si si) env arg1)
  (emit-heap-save 0)
  (emit-stack-load si)
  (emit-heap-save (next-hi 0))
  (emit-obj pair-tag)
  (emit-adjust-heap (next-hi (next-hi 0)))
)

(define-primitive (car si env arg)
  (emit-expr si env arg)
  (emit "    movq -1(%rax), %rax"))

(define-primitive (cdr si env arg)
  (emit-expr si env arg)
  (emit "    movq 7(%rax), %rax"))

(test-all)

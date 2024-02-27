#lang racket

(require rackunit)
(require "utils.rkt")
(provide all-defined-out)
(require racket/trace)


(define prog-check
  (λ (p ct)
    (match p
      (`(,(and `(class ,n . ,r) c) . ,p^)
       (let ((ct^ (hash-set ct n c)))
         (class-check n ct^)
         (prog-check p^ ct^)))
      (`(,e) (checker e empty-env ct)))))

(define checker
  (λ (e env ct)
    (match e
      (`,n #:when (number? n) (values e 'N))
      (`,b #:when (boolean? b) (values e 'B))
      (`(+ ,e1 ,e2) (let-values (((e1^ t1) (checker e1 env ct))
                                 ((e2^ t2) (checker e2 env ct)))
                      (check-type-eq? t1 'N e1)
                      (check-type-eq? t2 'N e2) 
                      (values e 'N)))  
      (`(- ,e1 ,e2) (let-values (((e1^ t1) (checker e1 env ct))
                                 ((e2^ t2) (checker e2 env ct)))
                      (check-type-eq? t1 'N e1)
                      (check-type-eq? t2 'N e2) 
                      (values e 'N)))
      (`(* ,e1 ,e2) (let-values (((e1^ t1) (checker e1 env ct))
                                 ((e2^ t2) (checker e2 env ct)))
                      (check-type-eq? t1 'N e1)
                      (check-type-eq? t2 'N e2) 
                      (values e 'N)))
      (`(if ,e1 ,r1 ,r2) (let-values (((e1^ te1) (checker e1 env ct))
                                      ((r1^ tr1) (checker r1 env ct))
                                      ((r2^ tr2) (checker r2 env ct)))
                                      (check-type-eq? te1 'B e)
                                      (check-type-eq? tr1 tr2 e)
                                      (values e tr1))) ;;since both have the same type, just return tr1
      (`(zero? ,x) (let-values (( (x^ tx) (checker x env ct)))
                     (check-type-eq? tx 'N x)
                     (values e 'B)))
      (`(new ,c . ,args)
       (let* ((arg-ts  (check-list args env ct))
              (f-ts (lookup-field c ct)))
         (if (not (equal? (length arg-ts) (length f-ts)))
             (error 'checker "not enough arguments in: ~a~n" e)
         (for ((f-t f-ts)
               (arg-t arg-ts))
           (check-type-eq? arg-t f-t e)))
         (values e c)))
      (`(send ,e₁ ,g . ,es)
       (let*-values (((e₁^ te₁) (checker e₁ env ct))
                    ((m-ts ) (m-type te₁ g ct))
                    ((e-ts ) (check-list es env ct)))
                    (for ((m-t m-ts)
                          (e-t e-ts))
                      (check-type-eq? m-t e-t e))
                    (values e (get-method-return-type te₁ g ct))))
      (`(/ ,e₁ ,f)
       (let*-values (((e₁^ te₁) (checker e₁ env ct))
                                 ((c ) (lookup-ct te₁ ct))
                                 ((fs ) (get-fs c))
                                 ((t-f ) (get-f-type fs f)))
                   (values e t-f)))
      (`(λ ,xs . (,body)) (let*-values (((env^ e^) (values (extend-types-env xs env) e))
                                          ((ts e^^) (values (get-arg-ts xs) e))
                                          ((body^ tbody) (checker body env^ ct)))
                            (values e (append `(→ ) ts `(,tbody)))))
      (`,y #:when (symbol? y) (values e (env y)))
      (`(let ,x-vs ,body) ;;TODO: FIX THIS 
       (let ((xs '())
             (t-xs '()))
         (for ((x-v-ls x-vs))
           (set! xs (cons (car x-v-ls) xs))
           (set! t-xs   (cons (cadr (checker (cadr x-v-ls) env ct) t-xs)))
         (let*-values (( (x^ tx) (check-list t-xs env ct))
                       ((env^ ) (extend-env* env xs tx))
                       ((body^ tbody) (checker body env^ ct)))
           (printf "tbody: ~a~n" tbody)
           (values e tbody)))))
      (`(,rator . ,rands)
       (let-values (((rator^ rator-ts) (checker rator env ct))
                    ((  rand-ts) (check-list rands env ct)))
         (check-type-eq? (minus-last-elem rator-ts) (append '(→ ) rand-ts) e)
         (values e rator-ts))))))

(define m-type
  (λ (cvar m ct)
    (match-let ((`(method (,_ self : ,_ . ,vars) : ,_ ,_) (lookup-method cvar m ct)))
      (get-arg-ts vars))))

(define get-method-return-type
  (λ (cvar m ct)
    (match-let ((`(method (,_ self : ,_ . ,_) : ,rt ,_)  (lookup-method cvar m ct)))
      rt)))



(define class-check
  (λ (c ct)
    (match-let ((`(class ,cvar (fields . ,fs) ,methods) (lookup-ct c ct)))
      (for ((m methods))
        (method-check cvar m ct)))))

(define method-check
  (λ (cvar m ct)
    (match-let ((`(method (,mvar . ,vars) : ,mt ,b ) m))
      (if (or (eqv? (caddr vars) 'Self) (eqv? (caddr vars) cvar))
          (checker b (extend-types-env vars empty-env) ct)
          (error 'method-check "~s≠self~n" (caddr vars))))))

(define extend-types-env
  (λ (xs env)
    (match xs
      (`(,x : ,tx) (extend-env env x tx))
      (`(,x : ,tx . ,r) (extend-types-env r (extend-env env x tx))))))


(define check-type-eq?
  (λ (t1 t2 e)
  (cond
    ((type-eq? t1 t2) #t)
    (else (error 'type-check "~a ≠ ~a\nin ~v" t1 t2 e)))))

(define lookup-ct
  (λ  (c c-def)
    (hash-ref c-def c)))

(define type-eq?
  (λ (t1 t2)
    (match `(,t1 . ,t2)
      (`(N . N) #t)
      (`(B . B) #t)
      (`(Self . Self) #t)
      (`(,C . ,C) #:when (symbol? C) #t)
      (`( (→ . ,ts1) . (→ . ,ts2)) (andmap type-eq? ts1 ts2))
      (else #f))))


(define lookup-field
  (lambda (c c-def)
    (match-let (( `(class ,_ (fields . ,fs) ,_ ) (lookup-ct c c-def)))
      (get-arg-ts fs))))

(define get-arg
  (λ (args fs f)
    (cond
      ((empty? args) (error 'get-arg "~s not found~n"f))
      ((eqv? (car fs) f) (car args))
      (else (get-arg (cdr args) (cdddr fs) f)))))

(define get-f-type
  (λ (fs f)
    (match fs
      ('() (error 'checker "field ~a not found~n" f))
      (`(,f-name : ,f-type . ,r) #:when(eqv? f-name f) f-type)
      (`(,f-name : ,f-type . ,r) (get-f-type r f)))))

(define get-fs
  (λ (c)
    (match-let ((`(class ,_ (fields . ,fs) . ,_) c))
      fs)))



(define get-arg-ts
  (λ (xs)
    (match xs
      ('() null)
      (`(,x : ,tx) (cons tx '()))
      (`(,x : ,tx . ,r) (cons tx (get-arg-ts r))))))

(define check-list
  (λ (ls env ct)
    (cond
      ((null? ls) null)
      (else (let-values (( (e ts)  (checker (car ls) env ct)))
              (cons ts (check-list (cdr ls) env ct)))))))

(define minus-last-elem
  (λ (ls) 
    (cond
      ((null? ls) null)
      ((null? (cdr ls)) null)
      (else (cons (car ls) (minus-last-elem (cdr ls)))))))
(define empty-env
  (λ (y) (error 'value-of-exp "unbound variable ~s" y)))



(check-equal? (let-values (( (v t)  (checker `(+ 1 (+ 1 2)) empty-env '()))) t) 'N)
(check-exn
 exn:fail?
 (λ () (let-values (( (v t)  (checker `(+ #f (+ 1 2)) empty-env '()))) t)))
(check-equal? (let-values (( (v t)  (checker `(- 1 (+ 1 2)) empty-env '()))) t) 'N)
(check-equal? (let-values (( (v t)  (checker `(- 1 (+ 1 (* 3 4))) empty-env '()))) t) 'N)
(check-exn
 exn:fail?
 (λ ()
   (checker `((λ (x : B) (if (zero? x) #t #f)) 2) empty-env '())))

(check-equal? (let-values (( (v t) (checker `((λ (x : N) (if (zero? x) #t #f)) 2) empty-env '()))) t) `(→ N B))

(check-exn
 exn:fail?
 (λ ()
   (checker `((λ (x : B) (if (zero? x) #t #f)) #t) empty-env '())))

(check-exn
 exn:fail?
 (λ ()
   (checker `((λ (x : N) (if (5) #t #f)) 0) empty-env '())))
(check-exn
 exn:fail?
 (λ ()
   (checker `((λ (x : B) (if (zero? x) #t #f)) 2) empty-env '())))
(check-exn
 exn:fail?
 (λ ()
   (checker `((λ (x : B y : N z : N) x) 1 #t 4) empty-env '())))

(check-equal? (let-values (( (v t) (checker `((λ (x : B y : N z : N) (if x (+ y z) 0)) #t 5 4) empty-env '()))) t) `(→ B N N N))

(define class-address
  `(class Adder
      (fields n : N)
      ((method (add-to-n self : Adder x : N y : N) : N
               (+ (+ x y) (/ self n))))))


(check-exn
 exn:fail?
 (λ ()
   (checker `(let ((x #t)) (zero? x)) empty-env '())))
(check-equal? (let-values (( (e te) (prog-check `(,class-address (new Adder 2))  (hash))))
                te) 'Adder)
(check-exn
 exn:fail?
 (λ ()
   (prog-check `(,class-address (new Adder #f))  (hash))))

(check-equal? (let-values (( (e te)
                             (prog-check`(,class-address
                                          (let ((myAdder (new Adder 7)))
                                            (+ 6 (/ myAdder n)))) (hash)))) te)
              'N)

(check-equal?  (let-values (( (e t) (prog-check test-prog-1 (hash)))) t) 'N)
(check-equal?  (let-values (( (e t) (prog-check test-prog-2 (hash)))) t) 'N)
(check-equal?  (let-values (( (e t) (prog-check test-prog-3 (hash)))) t) 'N)
(check-equal?  (let-values (( (e t) (prog-check test-prog-4 (hash)))) t) 'N)
(check-equal?  (let-values (( (e t) (prog-check test-prog-5 (hash)))) t) 'N)
(check-equal?  (let-values (( (e t) (prog-check test-prog-6 (hash)))) t) 'N)
(check-exn
 exn:fail?(λ () (prog-check test-prog-7 (hash))))
(check-equal?  (let-values (( (e t) (prog-check test-prog-8 (hash)))) t) 'B)
(check-equal?  (let-values (( (e t) (prog-check test-prog-9 (hash)))) t) 'N)
(check-exn
 exn:fail?(λ () (prog-check test-prog-10 (hash))))



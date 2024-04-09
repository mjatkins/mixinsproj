#lang racket

(require rackunit)
(require "utils.rkt")
(provide all-defined-out)
(require racket/trace)


(define prog-check
  (λ (p t)
    (match p
      (`(,(and `(mixin ,n . ,r) m-body) . ,p^)
       (add-mixin! t n m-body)
       (mixin-check n t)
       (prog-check p^ t))
      (`(,(and `(class ,n . ,r) c) . ,p^)
       (add-class! t n c)
       (prog-check p^ t))
      (`(,e)
       (for ((c-s (hash-keys (global-table-classes t))))
         (class-check c-s t))
       (checker e empty-env t)))))

(define class-check
  (λ (c ct)
    (match-let ((`(class ,cvar (fields . ,fs) (mix . ,mixes) ,methods) (get-class ct c)))
      (duplicate-methods-check (get-method-names-list methods) c)
      (duplicate-fields-check (get-fields-list fs) c)
      (for ((m methods))
        (method-check cvar m ct)))))

;;TODO: some checks for mixins:
;;DONE 1. Check that for all classes, there are no duplicate fields 
;;DONE 2. Check no duplicate methods in classes
;; 2.5: method-check uses class defintions for methods, and has no way at the moment to check just mixins. IDEA: specific helper function just for check method and check mixin.
;; 3. Check mixins being mixed as well (mix-check)
;; note that mixins are checked in linear order, as there can be no mutual recursion for mixins. 
;; mixin-check should yield a list of new field and method names.
;; mixin table should hold fields and methods. When merging self and mixin method and f names, we require it to not have dupes.
;; 4. make sure the error for dupes outputs the naming clash.

(define mixin-check
  (λ (m ct)
    (match-let ((`(mixin ,m-name (fields . ,fs) (mix . ,mixes) ,methods) (get-mixin ct m)))
      (mixin-methods-check mixes m ct)
      (mixin-fields-check mixes m ct)
      (for ((method methods))
        (method-check m-name method ct)))))

(define method-check/mixins
  (λ (mix-name m mixes t)
    (match-let ((`(method (,mvar . ,vars) : ,mt ,body ) m))
      (let ((env (extend-types-env vars empty-env)))
      (if (or (eqv? (caddr vars) 'Self) (eqv? (caddr vars) mix-name))
          (match body
            (`(send ,e₁ ,g . ,es)
             (let*-values (((e₁^ te₁) (checker e₁ env t))
                    ((m-ts ) (m-type te₁ g t))
                    ((e-ts ) (check-list es env t)))
                    (for ((m-t m-ts)
                          (e-t e-ts))
                      (check-type-eq? m-t e-t mix-name))
                    (values mix-name (get-method-return-type te₁ g t))))
            (`(/ ,e₁ ,f)
             (let*-values (((e₁^ te₁) (checker e₁ env t))
                                 ((c ) (get-mixin t te₁))
                                 ((fs ) (get-fs c))
                                 ((t-f ) (get-f-type fs f)))
               (values mix-name t-f)))
            (else (checker body (extend-types-env vars empty-env) t)))
          (error 'method-check/mixins "~s≠self~n" (caddr vars)))))))
      

(define checker
  (λ (e env ct)
    (match e
      (`,n #:when (number? n) (values e 'N))
      (`,b #:when (boolean? b) (values e 'B))
      (`,s #:when (string? s) (values e 'String))
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
              (f-ts (lookup-field c ct))
              (ms-list (get-mixins-from-class c ct))
              (total-fields-length (+ (length f-ts) (length (get-mixin-fields ms-list ct)))))
         (if (not (equal? (length arg-ts) total-fields-length))
             (error 'checker "# of args ≠ # of fields in: ~a~n" e)
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
                     ((c ) (if (class? ct te₁)
                               (get-class ct te₁)
                               (get-mixin ct te₁)))
                     ((fs ) (get-all-fs te₁ ct))
                     ((t-f ) (get-f-type fs f)))
                   (values e t-f)))
      (`(λ ,xs . (,body)) (let*-values (((env^ e^) (values (extend-types-env xs env) e))
                                          ((ts e^^) (values (get-arg-ts xs) e))
                                          ((body^ tbody) (checker body env^ ct)))
                            (values e (append `(→ ) ts `(,tbody)))))
      (`,y #:when (symbol? y) (values e (env y)))
      (`(let ,x-vs ,body)
       (let ((xs '())
             (t-xs '()))
         (for ((x-v-ls x-vs))
           (set! xs (cons (car x-v-ls) xs))
           (let-values  (((x tx) (checker (cadr x-v-ls) env ct)))
             (set! t-xs (cons tx t-xs))))
         (let*-values (( (env^ ) (extend-env* env xs t-xs))
                       ((body^ tbody) (checker body env^ ct)))
           (values e tbody))))
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





(define duplicate-fields-check
  (λ (fields c)
    (match fields
      (`() #f)
      (`(,a . ,d) (if (occurs? a d)
                      (error 'duplicate-field-check "duplicate field ~a in ~a~n" a c)          
                  (duplicate-fields-check d c))))))
(define mixin-methods-check
  (λ (mix-list m t)
    (let ([all-methods (get-method-names-list (get-mixin-methods (cons m mix-list) t))])
      (duplicate-methods-check all-methods m))))


(define mixin-fields-check
  (λ (mix-list m t)
    (let ([all-fields (get-fields-list (get-mixin-fields/types (cons m mix-list) t))])
      (duplicate-fields-check all-fields m))))

(define duplicate-methods-check
  (λ (m-list c)
    (match m-list
      (`() #f)
      (`(,a . ,d)
       (if (occurs? a d)
                      (error 'duplicate-methods-check "duplicate method ~a in ~a~n" a c)
                      (duplicate-methods-check d c))))))

(define get-method-names-list
  (λ (ms)
    (match ms
      (`((method (,name self : ,_ . ,vars) : ,_ . ,body) . ,r)
       (cons name (get-method-names-list r)))
      (`() null))))

(define get-fields-list
  (λ (ms)
    (match ms
      (`(,f-name : ,_ . ,r)
       (cons f-name (get-fields-list r)))
      (`() null))))

           

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
  (lambda (c t)
    (match-let (( `(class ,_ (fields . ,fs) (mix . ,_) ,_ ) (get-class t c)))
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

(define get-all-fs
  (λ (c-name t)
    (if (class? t c-name)
        (append (get-fs (get-class t c-name)) (get-mixin-fields/types (get-mixins-from-class c-name t) t))
        (get-mixin-fields/types `(,c-name) t))))





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
     (mix)
     ((method (add-to-n self : Adder x : N y : N) : N
              (+ (+ x y) (/ self n))))))


(check-exn
 exn:fail?
 (λ ()
   (checker `(let ((x #t)) (zero? x)) empty-env '())))
(check-equal? (let-values (( (e te) (prog-check `(,class-address (new Adder 2))  (empty-global-table))))
                te) 'Adder)
(check-exn
 exn:fail?
 (λ ()
   (prog-check `(,class-address (new Adder #f))  (empty-global-table))))

(check-equal? (let-values (( (e te)
                             (prog-check`(,class-address
                                          (let ((myAdder (new Adder 7)))
                                            (+ 6 (/ myAdder n)))) (empty-global-table)))) te)
              'N)

(check-equal?  (let-values (( (e t) (prog-check test-prog-1 (empty-global-table)))) t) 'N)
(check-equal?  (let-values (( (e t) (prog-check test-prog-2 (empty-global-table)))) t) 'N)
(check-equal?  (let-values (( (e t) (prog-check test-prog-3 (empty-global-table)))) t) 'N)
(check-equal?  (let-values (( (e t) (prog-check test-prog-4 (empty-global-table)))) t) 'N)
(check-equal?  (let-values (( (e t) (prog-check test-prog-5 (empty-global-table)))) t) 'N)
(check-equal?  (let-values (( (e t) (prog-check test-prog-6 (empty-global-table)))) t) 'N)
(check-exn
 exn:fail?(λ () (prog-check test-prog-7 (empty-global-table))))
(check-equal?  (let-values (( (e t) (prog-check test-prog-8 (empty-global-table)))) t) 'B)
(check-equal?  (let-values (( (e t) (prog-check test-prog-9 (empty-global-table)))) t) 'N)
(check-exn
 exn:fail?(λ () (prog-check test-prog-10 (empty-global-table))))
(check-equal?  (let-values (( (e t) (prog-check test-prog-11 (empty-global-table)))) t) 'B)
(check-equal?  (let-values (( (e t) (prog-check test-prog-12 (empty-global-table)))) t) 'B)
(check-equal?  (let-values (( (e t) (prog-check test-prog-13 (empty-global-table)))) t) 'N)
(check-equal?  (let-values (( (e t) (prog-check test-prog-14 (empty-global-table)))) t) 'String)
(check-equal?  (let-values (( (e t) (prog-check test-prog-16 (empty-global-table)))) t) 'N)
(check-exn
 exn:fail?(λ () (prog-check test-prog-17 (empty-global-table))))
(check-exn
 exn:fail?(λ () (prog-check test-prog-18 (empty-global-table))))
(check-exn
 exn:fail?(λ () (prog-check test-prog-19 (empty-global-table))))
(check-equal?  (let-values (( (e t) (prog-check test-prog-20 (empty-global-table)))) t) 'N)

(check-exn
 exn:fail?(λ () (prog-check test-prog-21 (empty-global-table))))


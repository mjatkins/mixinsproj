#lang racket
(require rackunit)
(require "utils.rkt")
;(require "typechecker.rkt")

(define lookup-ct
  (λ  (c c-def)
    (hash-ref c-def c)))

(define lookup-field
  (lambda (c f args c-def)
    (match-let (( `(class ,_ (fields . ,fs) ,_ ) (lookup-ct c c-def)))
      (get-arg args fs f))))

(define get-arg
  (λ (args fs f)
    (cond
      ((eqv? (car fs) f)
       (car args))
      (else (get-arg (cdr args) (cdddr fs) f)))))

(define value-of-list
  (λ (ls env c-def)
    (cond
      ((empty? ls) null)
      ((not (list? ls)) (value-of-exp ls env c-def))
      (else
       (cons (value-of-exp (car ls) env c-def) (value-of-list (cdr ls) env c-def))))))

(define apply-method 
  (lambda (self m m-args c-def)
    (match-let* (( `(method (,_ . ,formals) : ,_ ,body) m)
                   (env^ (extend-env*  empty-env formals `(,self . ,m-args))))
      (value-of-exp body env^ c-def))))


(define value-of-exp
  (λ (e env c-def)
    (match e
      (`,n #:when (number? n) n)
      (`,b #:when (boolean? b) b)
      (`,s #:when (string? s) s)
      (`(+ ,e1 ,e2)
       (+ (value-of-exp e1 env c-def)
          (value-of-exp e2 env c-def)))
      (`(- ,e1 ,e2)
       (- (value-of-exp e1 env c-def)
          (value-of-exp e2 env c-def)))
      (`(* ,e1 ,e2)
       (* (value-of-exp e1 env c-def)
          (value-of-exp e2 env c-def)))
      (`(if ,e1 ,r1 ,r2)
       (if (value-of-exp e1 env c-def) (value-of-exp r1 env c-def) (value-of-exp r2 env c-def)))
      (`(zero? ,x)
       (zero? (value-of-exp x env c-def)))
      (`(new ,c . ,args)
       (let ((val-args (value-of-list args env c-def)))
       `(object ,c . ,val-args)))
      (`(send ,e ,g . ,es)
       (match-let ([ (and `(object ,c . ,args) o)  (value-of-exp e env c-def)]
                   [m-args (value-of-list es env c-def)]) 
         (apply-method o (lookup-method c g c-def) m-args c-def)))
      (`(/ ,e ,f) ;;in other notation, e.f
       (match-let ([`(object ,c . ,args)
                    (value-of-exp e env c-def)])
         (lookup-field c f args c-def)))
      (`,y #:when (symbol? y) (env y))
      (`(λ ,xs . (,body)) (make-clos body xs env c-def))
      (`(let ,x-vs ,body)
       (let ((xs '())
             (vals '()))
         (for ((x-v-ls x-vs))
           (set! xs (cons (car x-v-ls) xs))
           (set! vals (cons (value-of-exp (cadr x-v-ls) env c-def) vals)))
         (value-of-exp body (extend-env* env xs vals) c-def)))
      (`(,rator . ,rands)
       (apply-clos
        (value-of-exp rator env c-def) (value-of-list rands env c-def))))))

(define make-clos
  (λ (body xs env c-def)
   `(make-clos ,body ,xs ,env ,c-def)))

(define apply-clos
  (λ (clos rands)
    (match clos
      (`(make-clos ,body ,xs ,env ,c-def) (value-of-exp body (extend-env* env xs rands) c-def)))))
(define empty-env
  (λ (y) (error 'value-of-exp "unbound variable ~s" y)))


(define value-of-prog
  (λ (p ct)
    (match p
      (`((class ,n . ,r ) . ,p^)
       (value-of-prog p^ (hash-set ct n `(class ,n . ,r))))
      (`(,e) (value-of-exp e empty-env ct)))))



    ;basic interpreter tests
(check-eqv? (value-of-exp '(+ 1 2) empty-env '()) 3)
(check-eqv? (value-of-exp `((λ (x : N) x) 2) empty-env '()) 2)
(check-eqv? (value-of-exp `((λ (x : B) (if (zero? x) #t #f)) 0) empty-env '()) #t)
(check-eqv? (value-of-exp `(let ((x 6)
                                  (y 7)
                                  (z 12))
                              (+ (+ x y) z)) empty-env (hash)) 25)
;;misc. tests
(check-eqv? (value-of-exp `(if #t 1 2) empty-env (hash)) 1)
(check-eqv? (value-of-exp `((λ (x : N y : N) (+ x y)) 2 3) empty-env '()) 5)
(check-eqv? (value-of-exp `((λ (a : N b : N c : N d : N) (+ b (+ a (+ c d))) ) 2 3 4 5) empty-env '()) 14)



(check-eqv? (value-of-prog test-prog-1 (hash)) 22)
(check-eqv? (value-of-prog test-prog-2 (hash)) 25)
(check-eqv? (value-of-prog test-prog-3 (hash)) 18)
(check-eqv? (value-of-prog test-prog-4 (hash)) 22)
(check-eqv? (value-of-prog test-prog-5 (hash)) 13)
(check-eqv? (value-of-prog test-prog-6 (hash)) 12)
(check-eqv? (value-of-prog test-prog-8 (hash)) #t)
(check-eqv? (value-of-prog test-prog-9 (hash)) 120)
;(check-eqv? (value-of-prog test-prog-11 (hash)) #t)
;(check-eqv? (value-of-prog test-prog-12 (hash)) #f)
(value-of-prog test-prog-13 (hash))

#lang racket
(require rackunit)
(require "utils.rkt")
(require racket/trace)



(define lookup-field
  (lambda (c f args t)
    (match-let (( `(class ,_ (fields . ,fs) (mix . ,ms) ,_ ) (get-class t c)))
      (let (( all_fields (append fs (get-mixin-fields/types ms t)))) ;;preserve ordering from mix cluase for appending to fs
        (get-arg args all_fields f)))))

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
  (λ (e env t)
    (match e
      (`,n #:when (number? n) n)
      (`,b #:when (boolean? b) b)
      (`,s #:when (string? s) s)
      (`(if ,e1 ,r1 ,r2)
       (if (value-of-exp e1 env t) (value-of-exp r1 env t) (value-of-exp r2 env t)))
      (`(new ,c . ,args)
       (let ((val-args (value-of-list args env t)))
       `(object ,c . ,val-args)))
      (`(send ,e ,g . ,es)
       (match-let ([ (and `(object ,c . ,args) o)  (value-of-exp e env t)]
                   [m-args (value-of-list es env t)])  
         (apply-method o (lookup-method c g t) m-args t)))
      (`(/ ,e ,f) ;;in other notation, e.f
       (match-let ([`(object ,c . ,args)
                    (value-of-exp e env t)])
         (lookup-field c f args t)))
      (`,y #:when (symbol? y) (env y))
      (`(λ ,xs . (,body)) (make-clos body xs env t))
      (`(let ,x-vs ,body)
       (let ((xs '())
             (vals '()))
         (for ((x-v-ls x-vs))
           (set! xs (cons (car x-v-ls) xs))
           (set! vals (cons (value-of-exp (cadr x-v-ls) env t) vals)))
         (value-of-exp body (extend-env* env xs vals) t)))
      (`(,p ,x) #:when (hash-has-key? unary-primitives p)
                ((hash-ref unary-primitives p) (value-of-exp x env t)))
      (`(,p ,e1 ,e2) #:when (hash-has-key? binary-primitives p)
                     ((hash-ref binary-primitives p) (value-of-exp e1 env t)
                                                     (value-of-exp e2 env t)))
      (`(,rator . ,rands)
       (apply-clos
        (value-of-exp rator env t) (value-of-list rands env t))))))

(define make-clos
  (λ (body xs env t)
   `(make-clos ,body ,xs ,env ,t)))

(define apply-clos
  (λ (clos rands)
    (match clos
      (`(make-clos ,body ,xs ,env ,t) (value-of-exp body (extend-env* env xs rands) t)))))
(define empty-env
  (λ (y) (error 'value-of-exp "unbound variable ~s" y)))


(define value-of-prog
  (λ (p t)
    (match p
      (`((mixin ,n . ,r ) . ,p^)
        (add-mixin! t n `(mixin ,n . ,r))
        (value-of-prog p^ t))
       (`((class ,n . ,r ) . ,p^)
        (add-class! t n `(class ,n . ,r))
        (value-of-prog p^ t))
       (`(,e) (value-of-exp e empty-env t)))))



    ;basic interpreter tests
(check-eqv? (value-of-exp '(+ 1 2) empty-env '()) 3)
(check-eqv? (value-of-exp `((λ (x : N) x) 2) empty-env '()) 2)
(check-eqv? (value-of-exp `((λ (x : B) (if (zero? x) #t #f)) 0) empty-env '()) #t)
(check-eqv? (value-of-exp `(let ((x 6)
                                  (y 7)
                                  (z 12))
                              (+ (+ x y) z)) empty-env (empty-global-table)) 25)
;;misc. tests
(check-eqv? (value-of-exp `(if #t 1 2) empty-env (empty-global-table)) 1)
(check-eqv? (value-of-exp `((λ (x : N y : N) (+ x y)) 2 3) empty-env '()) 5)
(check-eqv? (value-of-exp `((λ (a : N b : N c : N d : N) (+ b (+ a (+ c d))) ) 2 3 4 5) empty-env '()) 14)



(check-eqv? (value-of-prog test-prog-1 (empty-global-table)) 22)
(check-eqv? (value-of-prog test-prog-2 (empty-global-table)) 25)
(check-eqv? (value-of-prog test-prog-3 (empty-global-table)) 18)
(check-eqv? (value-of-prog test-prog-4 (empty-global-table)) 22)
(check-eqv? (value-of-prog test-prog-5 (empty-global-table)) 13)
(check-eqv? (value-of-prog test-prog-6 (empty-global-table)) 12)
(check-eqv? (value-of-prog test-prog-8 (empty-global-table)) #t)
(check-eqv? (value-of-prog test-prog-9 (empty-global-table)) 120)
(check-eqv? (value-of-prog test-prog-11 (empty-global-table)) #t)
(check-eqv? (value-of-prog test-prog-12 (empty-global-table)) #f)
(check-eqv? (value-of-prog test-prog-13 (empty-global-table)) 14)
(check-eqv? (value-of-prog test-prog-14 (empty-global-table)) "foobar-town")
(check-eqv?  (value-of-prog test-prog-15 (empty-global-table)) 1)
(check-eqv?  (value-of-prog test-prog-16 (empty-global-table)) 3)
(check-eqv? (value-of-prog test-prog-20 (empty-global-table)) 1)



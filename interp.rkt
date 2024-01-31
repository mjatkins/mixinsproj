#lang racket
(require rackunit)
(require "typechecker.rkt")

(define lookup-ct
  (λ  (c c-def)
    (hash-ref c-def c)))

(define extend-env*
  (λ (env xs args )
    (match `(,xs . (,args))
      ( `(() . ,_) env)
      ( `(,_ . ()) env)
      ( `( (,x : ,t . ,xd) (,arg . ,ad))
        (extend-env* (extend-env env x arg) xd ad ))
      (`((,x : ,t . ,r) . (,arg))
       (extend-env env x arg))
      ( `((,x) . (,arg))
        (extend-env env x arg)))))


                           
        
(define lookup-field
  (lambda (c f args c-def)
    (match-let (( `(class-value (fields . ,fs) ,_ ) (lookup-ct c c-def)))
      (get-arg args fs f))))

(define get-arg
  (λ (args fs f)
    (cond
      ((eqv? (car fs) f) (car args))
      (else (get-arg (cdr args) (cdr fs) f)))))
    
(define lookup-method
  (λ (c g c-def)
    ;; class is defined as:
    ;; (class-value (fields ...) ((method (g x ...) e ) ... ))
    (match-let (( `(class-value ,_ . (,methods)) (lookup-ct c c-def)))
      (findf (λ (m) (match-let (( `(method  (,g^ . ,_) ,body) m) )
                      (eqv? g g^))) methods))))

(define value-of-list
  (λ (ls env c-def)
    (cond
      ((empty? ls) null)
      ((not (list? ls)) (value-of-exp ls env c-def))
      (else (cons (value-of-exp (car ls) env c-def) (value-of-list (cdr ls) env c-def))))))

(define apply-method
  (lambda (self m m-args c-def)
    (match-letrec (( `(method (,_ . ,formals) ,body) m)
                   (env^ (extend-env*  empty-env formals `(,self . ,m-args))))
      (value-of-exp body env^ c-def))))    

(define extend-env
  (λ (env x arg) 
    (λ (y)
      (cond
        ((eqv? y x) arg)
        (else (env y))))))


(define value-of-exp
  (λ (e env c-def)
    (match e
      (`,n #:when (number? n) n)
      (`,b #:when (boolean? b) b)
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
      (`(let ((,x ,v)) ,body)
       (let ((val (value-of-exp v env c-def)))
         (value-of-exp body (extend-env env x val) c-def)))
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
       (value-of-prog p^ (hash-set ct n `(class-value . ,r))))
      (`(,e) (value-of-exp e empty-env ct)))))




    ;basic interpreter tests
(check-eqv? (value-of-exp '(+ 1 2) empty-env '()) 3)
(check-eqv? (value-of-exp `((λ (x : N) x) 2) empty-env '()) 2)
(check-eqv? (value-of-exp `((λ (x : B) (if (zero? x) #t #f)) 0) empty-env '()) #t)
;;misc. tests
(check-eqv? (value-of-exp `(if #t 1 2) empty-env (hash)) 1)
(check-eqv? (value-of-exp `((λ (x : N y : N) (+ x y)) 2 3) empty-env '()) 5)
(check-eqv? (value-of-exp `((λ (a : N b : N c : N d : N) (+ b (+ a (+ c d))) ) 2 3 4 5) empty-env '()) 14)

(define test-prog-1
  `((class Dog
       (fields sound1 sound2)
       ((method (bark1 self : Dog)
                (/ self sound1))
        (method (bark2 self : Dog)
                (/ self sound2))))
    (let ((boston-terrier (new Dog 1 22)))
      (send boston-terrier bark2))))

(define test-prog-2
  `((class Rectangle
      (fields x y)
      ((method (area self : Rectangle) ;;get area of the square
               (* (/ self x) (/ self y)))))
    
  (let ((myRectangle (new Rectangle 5 5)))
    (send myRectangle area))))

(define test-prog-3
  `((class Adder
      (fields n)
      ((method (add-to-n self : Adder x : N y : N)
               (+ (+ x y) (/ self n)))))
    (let ((myAdder (new Adder 7)))
      (send myAdder add-to-n 5 6))))

(define test-prog-4
  `((class Dog
       (fields sound1 sound2)
       ((method (bark1 self : Dog)
                (/ self sound1))
        (method (bark2 self : Dog)
                (/ self sound2))))
    (let ((boston-terrier (new Dog 1 (+ 10 12))))
      (send boston-terrier bark2))))

(check-eqv? (value-of-prog test-prog-1 (hash)) 22)
(check-eqv? (value-of-prog test-prog-2 (hash)) 25)
(check-eqv? (value-of-prog test-prog-3 (hash)) 18)
(check-eqv? (value-of-prog test-prog-4 (hash)) 22)

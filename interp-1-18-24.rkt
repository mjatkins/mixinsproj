#lang racket
(require rackunit)

(define test-ct
  (make-hash
  `((Dog .  ;;key
  (class-value (fields sound1 sound2) ((method (bark1 self)
                                         (/ self sound1))
                                 (method (bark2 self)
                                         (/ self sound2))))))))
(hash-set! test-ct 'Counter `(class-value (fields count) ((method (increment self n)
                                 (+ (/ self count) n)))))

(define lookup-ct
  (λ  (c c-def)
    (hash-ref c-def c)))

(define extend-env*
  (λ (env xs args c-def)
    (match `(,xs . (,args))
      ( `(() . ,_) env)
      ( `(,_ . ()) env)
      ( `( (,x . ,xd) (,arg . ,ad))
        (extend-env* (extend-env env x arg c-def) xd ad c-def))
      ( `(,x . ,arg)
        (extend-env env x arg c-def)))))

                           
        
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
    ;; (class (fields ...) ((method (g x ...) e ) ... ))
    (match-let (( `(class-value ,_ . (,methods)) (lookup-ct c c-def)))
      (findf (λ (m) (match-let (( `(method  (,g^ . ,_) ,body) m) )
                      (eqv? g g^))) methods))))


(define apply-method
 (lambda (self m m-args c-def)
   (match-letrec (( `(method (,_ . ,formals) ,body) m)
                  (env^ (extend-env*  empty-env formals `(,self . ,m-args) c-def)))
       (value-of-exp body env^ c-def))))    

(define extend-env
  (λ (env x arg c-def) 
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
      (`(if (,pred ,e1) ,r1 ,r2)
       (if (value-of-exp `(,pred ,e1) env c-def) (value-of-exp r1 env c-def) (value-of-exp r2 env c-def)))
      (`(zero? ,x)
       (zero? (value-of-exp x env c-def)))
      (`(new ,c . ,args)
       `(object ,c . ,args))
      (`(send ,e ,g . ,es)
       (match-let ([ (and `(object ,c . ,args) o)  (value-of-exp e env c-def)]
                   [m-args (map (lambda (e)
                                  (value-of-exp e env c-def)) es)])
         (apply-method o (lookup-method c g c-def) m-args c-def)))             
      (`(/ ,e ,f) ;;in other notation, e.f
       (match-let ([`(object ,c . ,args)
                    (value-of-exp e env c-def)])
         (lookup-field c f args c-def)))
      (`,y #:when (symbol? y) (env y))
      (`(λ (,x) ,body)
       #:when (symbol? x)  
       (λ (arg)
         (value-of-exp body
                       (extend-env env x arg c-def)
                       c-def)))
      (`(let ((,x ,v)) ,body)
       (let ((val (value-of-exp v env c-def)))
         (value-of-exp body (extend-env env x val c-def) c-def)))
      (`(,rator ,rand)
       ((value-of-exp rator env c-def) (value-of-exp rand env c-def))))))

(define empty-env
  (λ (y) (error 'value-of-exp "unbound variable ~s" y)))




(define value-of-prog
  (λ (p ct)
    (match p
      (`((class ,n . ,r ) . ,p^)
       (value-of-prog p^ (hash-set ct n `(class-value . ,r))))
      (`(,e) (value-of-exp e empty-env ct)))))




    ;basic interpreter tests
(check-eqv? (value-of-exp '(+ 1 2) empty-env test-ct) 3)
(check-eqv? (value-of-exp `((λ (x) x) 2) empty-env test-ct) 2)
(check-eqv? (value-of-exp `((λ (x) (if (zero? x) #t #f)) 0) empty-env test-ct) #t)
;field tests
(check-eqv? (value-of-exp `(let ((c (new Counter 5))) (/ c count) ) empty-env test-ct) 5)
;;method tests
(check-eqv? (value-of-exp `(let ((c (new Counter 5))) (send c increment 10)) empty-env test-ct) 15)
(check-eqv? (value-of-exp `(let ((boston-terrier (new Dog 1 22))) (send boston-terrier bark1)) empty-env test-ct) 1)
(check-eqv? (value-of-exp `(let ((boston-terrier (new Dog 1 22))) (send boston-terrier bark2)) empty-env test-ct) 22)

(define test-prog-1
  `((class Dog
       (fields sound1 sound2)
       ((method (bark1 self)
                (/ self sound1))
        (method (bark2 self)
                (/ self sound2))))
    (let ((boston-terrier (new Dog 1 22)))
      (send boston-terrier bark2))))

(value-of-prog test-prog-1 (hash))


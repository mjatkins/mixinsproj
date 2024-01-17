#lang racket



(define test-ct
  (make-hash
  `((Counter .  ;;key
  (class (fields count) ((method (increment self n))
                          (+ (/ self count) n))))))) ;;value
(define lookup-ct
  (λ  (c c-def)
    (hash-ref c-def c)))

(define extend-env*
  (λ (env xs args)
    (match `(,xs . ,args)
      ( `(() . ,_) env)
      ( `(,_ . ()) env)
      ( `( (,x . ,xd) (,arg . ,ad))
        (extend-env* (extend-env x arg) xd ad)))))
                           
        
(define lookup-field
  (lambda (c f args c-def)
    (match-let (( `(class (fields . ,fs) ,_ ) (lookup-ct c c-def)))
      (get-arg args fs f))))

(define get-arg
  (λ (args fs f)
    (cond
      ((eqv? (car fs) f) (car args))
      (else (get-arg (cdr args) (cdr fs) f)))))
    
(define lookup-method
  (λ (c g c-def)
    (match-let (( `(class ,_ (method . ,methods) ) (lookup-ct c c-def)))
      (findf (λ (m) (match-let ((`(method  (,g^ . ,_) ,_ ) m) )
               (eqv? g g^)) methods)))))
(define apply-method
 (lambda (self m m-args)
   (match-let (( `(method (,_ . ,formals) ,body) m))
     (match-let ((env^ (extend-env* empty-env formals m-args)))
     
     
     

     
     

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
      (`(send ,e ,g ,es)
       (match-let ([ (and `(object ,c . ,args) o)  (value-of-exp e env c-def)]
                   [m-args (map (lambda (e)
                                  (value-of-exp e env c-def)) es)])
         (apply-method o (lookup-method c g c-def) m-args)))             
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
  (λ (y) (error 'value-of "unbound variable ~s" y)))
;basic interpreter tests
;(value-of-exp '(+ 1 2) empty-env test-ct)
;(value-of-exp `((λ (x) x) 2) empty-env test-ct)
;(value-of-exp `((λ (x) (if (zero? x) #t #f)) 0) empty-env test-ct)
;field tests
;(value-of-exp `(new Counter 5 4) empty-env test-ct)
;(value-of-exp `(let ((c (new Counter 5))) (/ c count) ) empty-env test-ct)


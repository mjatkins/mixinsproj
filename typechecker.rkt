#lang racket
(require rackunit)
(provide all-defined-out)

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
                     (check-type-eq? tx 'B x)
                     (values e 'B)))
      (`(new ,c . ,args) e)
      (`(send ,e₁ ,g . ,es) e)            
      (`(/ ,e₁ ,f) e)
      (`(λ ,xs . (,body)) e)
      (`,y #:when (symbol? y) (values e (env y)))
      (`(let ((,x ,v)) ,body) e)
      (`(,rator ,rands) e))))


(define check-type-eq?
(λ (t1 t2 e)
  (cond
    ((type-eq? t1 t2) #t)
    (else (error 'type-check "~a != ~a\nin ~v" t1 t2 e)))))

(define type-eq?
  (λ (t1 t2)
    (match `(,t1 . ,t2)
      (`(N . N) #t)
      (`(B . B) #t)
      (`(Self . Self) #t)
      (`(,C . ,C) #:when (symbol? C) #t)
      (`( (→ . ,ts1) . (→ . ,ts2)) (map type-eq? ts1 ts2))
      (else #f))))


(check-equal? (let-values (( (v t)  (checker `(+ 1 (+ 1 2)) '() '()))) t) 'N)
(check-exn
 exn:fail?
 (λ () (let-values (( (v t)  (checker `(+ #f (+ 1 2)) '() '()))) t)))
(check-equal? (let-values (( (v t)  (checker `(- 1 (+ 1 2)) '() '()))) t) 'N)
(check-equal? (let-values (( (v t)  (checker `(- 1 (+ 1 (* 3 4))) '() '()))) t) 'N)

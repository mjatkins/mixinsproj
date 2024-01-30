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
      (`(- ,e1 ,e2) e)
      (`(* ,e1 ,e2) e)
      (`(if ,e1 ,r1 ,r2) e)
      (`(zero? ,x) e)
      (`(new ,c . ,args) e)
      (`(send ,e₁ ,g . ,es) e)            
      (`(/ ,e₁ ,f) e)
      (`,y #:when (symbol? y) (values e (env y)))
      (`(λ ,x ,body) e)
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
(let-values (( (v t)  (checker `(+ #f (+ 1 2)) '() '()))) t)
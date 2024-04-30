#lang racket
(require rackunit)
(require "interp.rkt")
(require "typechecker.rkt")



(check-eqv? (value-of-prog `((+ 1 2)) (empty-global-table)) 3)
(check-eqv? (value-of-prog `(((λ (x : N) x) 2)) (empty-global-table))  2)
(check-eqv? (value-of-prog `(((λ (x : B) (if (zero? x) #t #f)) 0))   (empty-global-table)) #t)
(check-eqv? (value-of-prog `((let ((x 6)
                                  (y 7)
                                  (z 12))
                              (+ (+ x y) z)))   (empty-global-table)) 25)
(check-eqv? (value-of-prog `((if #t 1 2))   (empty-global-table)) 1)
(check-eqv? (value-of-prog `(((λ (x : N y : N) (+ x y)) 2 3))   (empty-global-table)) 5)
(check-eqv? (value-of-prog `(((λ (a : N b : N c : N d : N) (+ b (+ a (+ c d))) ) 2 3 4 5))  (empty-global-table)) 14)

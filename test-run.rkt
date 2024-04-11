#lang racket
(require "typechecker.rkt")
(require "interp.rkt")

(let ([contents (file->list "test/test_prog_2.rkt")])
  (printf "test case: ~a~n" contents)
  (prog-check contents (empty-global-table)))


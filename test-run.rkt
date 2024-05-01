#lang racket
(require "typechecker.rkt")
(require "interp.rkt")
(require rackunit)
;;runs all files in the ./tests/ directory. ".tyerr" refers to files that shouldn't pass the typechecker, ".rktn" is for files that return a number (8 to pass the test case), and ".rkts" is for programs that return a string ""

(define run-value-tests
  (λ ()
    (let ([number-test-list '()])
      (for ((file (directory-list "./test")))
        (when (regexp-match #rx".*[.]rktn$" file)
          (set! number-test-list (cons (string-append "test/" (path->string file)) number-test-list))))
      (printf "List of Number tests: ~a~n" number-test-list)
      (for ((file number-test-list))
        (let ([contents (file->list file)]) 
          (begin
            (prog-check contents (empty-global-table))
            (check-eqv? (value-of-prog contents (empty-global-table)) 8 (string-append "TEST FILE: " file))))))
    
    (let ([string-test-list '()])
      (for ((file (directory-list "./test")))
        (when (regexp-match #rx".*rkts$" file)
          (set! string-test-list (cons (string-append "test/" (path->string file)) string-test-list))))
      (printf "List of String tests: ~a~n" string-test-list)
      (for ((file string-test-list))
        (let ([contents (file->list file)])
          (begin
            (prog-check contents (empty-global-table))
            (check string=?  (value-of-prog contents (empty-global-table)) "hello world"  (string-append "TEST FILE: " file))))))))



(define run-type-error-tests
  (λ ()
    (let ([tyerr-list '()])
      (for ((file (directory-list "./test")))
        (when (regexp-match #rx".*tyerr$" file)
          (set! tyerr-list (cons (string-append "test/" (path->string file)) tyerr-list))))
      (printf "List of type error tests: ~a~n" tyerr-list)
      (for ((file tyerr-list))
        (let ([contents (file->list file)])
          (check-exn
           exn:fail?
           (λ ()
             (prog-check contents (empty-global-table))) (string-append "TEST FILE: " file)))))))

(define one-file-test
  ;;function used to test one test, lets us see the error message
  (λ (path test-type)
    (cond
      ((eqv? 'I test-type)
       (value-of-prog (file->list path) (empty-global-table)))
      ((eqv? 'T test-type)
       (prog-check (file->list path) (empty-global-table)))
      (else
       (display "incorrect test type")))))

(run-value-tests)
(run-type-error-tests)




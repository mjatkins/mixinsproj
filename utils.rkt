#lang racket
(provide lookup-method lookup-ct class-address class-rectangle extend-env extend-env* test-prog-1 test-prog-2 test-prog-3 test-prog-4
         test-prog-5 test-prog-6 test-prog-7 test-prog-8 test-prog-9 test-prog-10 test-prog-11 test-prog-12 test-prog-13 test-prog-14)


;;shared functions

(define lookup-method
  (λ (c g c-def)
    ;; class is defined as:
    ;; (class-value (fields ...) ((method (g x ...) e ) ... ))
    (match-let (( `(class ,_ (fields . ,_) . (,methods)) (lookup-ct c c-def)))
      (findf (λ (m) (match-let (( `(method  (,g^ . ,_) : ,_ ,body) m) )
                      (eqv? g g^))) methods))))
(define extend-env
  (λ (env x arg) 
    (λ (y)
      (cond
        ((eqv? y x) arg)
        (else (env y))))))

(define extend-env*
  (λ (env xs args)
    (match `(,xs . (,args))
      ( `(() . ,_) env)
      ( `(,_ . ()) env)
      ( `((,x : ,t . ,xd) (,arg . ,ad))
        (extend-env* (extend-env env x arg) xd ad ))
      ( `((,x  . ,xd) (,arg . ,ad))
        (extend-env* (extend-env env x arg) xd ad ))
      (`((,x : ,t . ,r) . (,arg))
       (extend-env env x arg))
      ( `((,x) . (,arg))
        (extend-env env x arg)))))


(define lookup-ct
  (λ  (c c-def)
    (hash-ref c-def c)))

;;class-definitions

(define class-address
  `(class Adder
      (fields n : N)
      ((method (add-to-n self : Adder x : N y : N) : N
               (+ (+ x y) (/ self n))))))
(define broken-class
  `(class Broken
     (fields boolean : B number : N)
     ((method (add-some-booleans self : Broken bool2 : B) : N
              (+ bool2 (/ self boolean))))))

(define class-rectangle
  `(class Rectangle
      (fields x : N y : N)
      ((method (area self : Rectangle) : N ;;get area of the square
               (* (/ self x) (/ self y))))))

(define class-car
  `(class Car
     (fields model-number : N new? : B miles-driven : N)
     ((method (drive self : Car miles : N) : Car
              (new Car (/ self model-number) #f (+ (/ self miles-driven) miles)))
      (method (make-new-model self : Car new-model-number : N) : Car
              (new Car new-model-number #t 0)))))

(define class-fact
  `(class Fact
     (fields n : N total : N)
     ((method (factorial self : Fact) : N
              (if (zero? (/ self n))
                  (/ self total)
              (send (new Fact (- (/ self n ) 1) (* (/ self n) (/ self total))) factorial))))))
(define even-class
  `(class Even
     (fields x : N)
     ((method (even? self : Even) : B
              (if (zero? (/ self x))
                  #t
                  (send (new Odd (- (/ self x) 1)) even?))))))
(define odd-class
      `(class Odd
         (fields x : N)
         ((method (even? self : Odd) : B
                  (if (zero? (/ self x))
                      #f
                      (send (new Even (- (/ self x) 1)) even?))))))
(define city-class
  `(class City
     (fields name : String population : N)
     ((method (increasePopulation self : City new-residents : N) : City
      (new City (/ self name) (+ (/ self population) new-residents))))))

;;test programs

(define test-prog-1
  `((class Dog
       (fields sound1 : N sound2 : N)
       ((method (bark1 self : Dog) : N
                (/ self sound1))
        (method (bark2 self : Dog) : N
                (/ self sound2))))
    (let ((boston-terrier (new Dog 1 22)))
      (send boston-terrier bark2))))

(define test-prog-2
  `((class Rectangle
      (fields x : N y : N)
      ((method (area self : Rectangle) : N ;;get area of the square
               (* (/ self x) (/ self y)))))

  (let ((myRectangle (new Rectangle 5 5)))
    (send myRectangle area))))

(define test-prog-3
  `(,class-address
    (let ((myAdder (new Adder 7)))
      (send myAdder add-to-n 5 6))))

(define test-prog-4
  `((class Dog
       (fields sound1 : N sound2 : N)
       ((method (bark1 self : Dog) : N
                (/ self sound1))
        (method (bark2 self : Dog) : N
                (/ self sound2))))
    (let ((boston-terrier (new Dog 1 (+ 10 12))))
      (send boston-terrier bark2))))

(define test-prog-5
  `(,class-address
    (let ((myAdder (new Adder 7)))
      (+ 6 (/ myAdder n)))))

(define test-prog-6
  `(,class-address ,class-rectangle
                   (let ((o1 (new Rectangle 5 6)))
                     (let ((o2 (new Adder 7)))
                       (+ (/ o1 x) (/ o2 n))))))
(define test-prog-7
  `(,broken-class
    (λ (x) x) 2))

(define test-prog-8
  `(,class-car
    (let ((car1 (new Car 10000 #t 0)))
      (let ((car2 (send car1 make-new-model 10001)))
        (/ car2 new?)))))

(define test-prog-9
  `(,class-fact
    (let ((myFact (new Fact 5 1)))
      (send myFact factorial))))

(define test-prog-10 ;;this shouldn't work, as it doesn't supply enough args
  `(,class-fact
    (let ((myFact (new Fact 5)))
      (send myFact factorial))))

;;mutual recursion tests
(define test-prog-11
  `(,even-class ,odd-class
    (let ((myNum (new Even 6)))
      (send myNum even?))))

(define test-prog-12
  `(,even-class ,odd-class
                (let ((myNum (new Even 7)))
                  (send myNum even?))))

;;multiple let vars test
(define test-prog-13
  `(,class-address
    (let ((myAdder (new Adder 7))
          (myAdder2 (new Adder 6)))
      (+ (/ myAdder n) (/ myAdder n)))))

(define test-prog-14
  `(,city-class
    (let ((myCity (new City "foobar-town" 7)))
      (/ myCity name))))

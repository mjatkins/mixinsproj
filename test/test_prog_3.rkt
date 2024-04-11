(class Adder
     (fields n : N)
     (mix)
      ((method (add-to-n self : Adder x : N y : N) : N
               (+ (+ x y) (/ self n)))))
(let ((myAdder (new Adder 7)))
      (send myAdder add-to-n 5 6))

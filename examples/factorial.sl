;; Integers have no fixed size. They overflow from native ints to big integers on their own, so
;; nothing here needs to know how large the answers get.

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; A tail-recursive Fibonacci: the VM reuses the frame, so this runs in constant stack.
(define (fibonacci n)
  (define (go a b k)
    (if (= k 0) a (go b (+ a b) (- k 1))))
  (go 0 1 n))

(define (digit-sum n)
  (if (= n 0)
      0
      (+ (rem n 10) (digit-sum (quot n 10)))))

(print (factorial 20))
(print (factorial 30))
(print (factorial 100))
(print (fibonacci 300))
(print (digit-sum (factorial 1000)))
(print (quot (factorial 100) (factorial 98)))

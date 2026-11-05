(define (average xs)
  (quot (sum xs) (length xs)))

(define (report xs)
  (print (average xs)))

(report '(1 2 3))
(report nil)

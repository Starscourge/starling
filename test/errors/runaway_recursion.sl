(define (forever n)
  (+ 1 (forever (+ n 1))))

(print (forever 0))

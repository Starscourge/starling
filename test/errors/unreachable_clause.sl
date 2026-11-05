(define (sign n)
  (match n
    (0 "zero")
    (_ "nonzero")
    (1 "one")))

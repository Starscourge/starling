(define (both a b)
  (match (pair a b)
    ((pair (some x) (some y)) (+ x y))
    ((pair none _) 0)))

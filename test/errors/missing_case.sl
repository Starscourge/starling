(type shape
  (circle int)
  (square int)
  (triangle int int))

(define (area s)
  (match s
    ((circle r) (* 3 (* r r)))
    ((square w) (* w w))))

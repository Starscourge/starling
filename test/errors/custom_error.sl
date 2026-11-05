(define (first xs)
  (match xs
    ((cons x _) x)
    (nil (error "first: the list is empty"))))

(print (+ 1 (first nil)))

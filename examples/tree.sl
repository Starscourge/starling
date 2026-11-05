;; A polymorphic binary search tree. No function needs a type annotation: inference finds the most
;; general type for each one, and `starling check` will print them.

(type (tree a)
  leaf
  (node (tree a) a (tree a)))

;; The comparison is a parameter, so the tree works for any element type.
(define (insert less x t)
  (match t
    (leaf (node leaf x leaf))
    ((node l v r)
     (cond ((less x v) (node (insert less x l) v r))
           ((less v x) (node l v (insert less x r)))
           (else t)))))

(define (from-list less xs)
  (foldl (lambda (t x) (insert less x t)) leaf xs))

(define (to-list t)
  (match t
    (leaf nil)
    ((node l v r) (append (to-list l) (cons v (to-list r))))))

(define (height t)
  (match t
    (leaf 0)
    ((node l _ r) (+ 1 (max (height l) (height r))))))

(define (contains? less x t)
  (match t
    (leaf false)
    ((node l v r)
     (cond ((less x v) (contains? less x l))
           ((less v x) (contains? less x r))
           (else true)))))

(define numbers (from-list < '(50 30 70 20 40 60 80 30)))

(print (to-list numbers))
(print (height numbers))
(print (contains? < 60 numbers))
(print (contains? < 65 numbers))

;; The same code at a different type. Strings compare by length here.
(define (shorter a b) (< (string-length a) (string-length b)))
(define words (from-list shorter '("pear" "fig" "banana" "cherry!")))

(print (to-list words))

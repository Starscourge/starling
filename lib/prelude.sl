;; The standard library, written in Starling and loaded before every program.

(type (option a) none (some a))
(type (pair a b) (pair a b))

;; Functions

(define (id x) x)
(define (const x y) x)
(define (compose f g) (lambda (x) (f (g x))))
(define (flip f) (lambda (a b) (f b a)))

;; Integers

(define (abs n) (if (< n 0) (neg n) n))
(define (min a b) (if (< a b) a b))
(define (max a b) (if (> a b) a b))
(define (even? n) (= (rem n 2) 0))
(define (odd? n) (not (even? n)))
(define (gcd a b) (if (= b 0) (abs a) (gcd b (rem a b))))

;; Floored division. quot and rem round toward zero instead.
(define (mod a b)
  (let ((r (rem a b)))
    (if (or (= r 0) (= (< r 0) (< b 0))) r (+ r b))))

(define (div a b) (quot (- a (mod a b)) b))

(define (pow base exponent)
  (cond ((< exponent 0) (error "pow: negative exponent"))
        ((= exponent 0) 1)
        ((even? exponent) (let ((half (pow base (quot exponent 2)))) (* half half)))
        (else (* base (pow base (- exponent 1))))))

;; Pairs

(define (fst p) (match p ((pair a _) a)))
(define (snd p) (match p ((pair _ b) b)))

;; Options

(define (some? o) (match o ((some _) true) (none false)))

(define (with-default default o)
  (match o
    ((some x) x)
    (none default)))

(define (map-option f o)
  (match o
    ((some x) (some (f x)))
    (none none)))

;; Lists

(define (null? xs)
  (match xs
    (nil true)
    (_ false)))

(define (head xs)
  (match xs
    ((cons x _) x)
    (nil (error "head: empty list"))))

(define (tail xs)
  (match xs
    ((cons _ rest) rest)
    (nil (error "tail: empty list"))))

(define (foldl f acc xs)
  (match xs
    (nil acc)
    ((cons x rest) (foldl f (f acc x) rest))))

(define (foldr f acc xs)
  (match xs
    (nil acc)
    ((cons x rest) (f x (foldr f acc rest)))))

(define (length xs) (foldl (lambda (n _) (+ n 1)) 0 xs))
(define (reverse xs) (foldl (lambda (acc x) (cons x acc)) nil xs))
(define (sum xs) (foldl + 0 xs))
(define (product xs) (foldl * 1 xs))

(define (append xs ys)
  (match xs
    (nil ys)
    ((cons x rest) (cons x (append rest ys)))))

(define (map f xs)
  (match xs
    (nil nil)
    ((cons x rest) (cons (f x) (map f rest)))))

(define (filter keep? xs)
  (match xs
    (nil nil)
    ((cons x rest)
     (if (keep? x)
         (cons x (filter keep? rest))
         (filter keep? rest)))))

(define (for-each f xs)
  (match xs
    (nil unit)
    ((cons x rest)
     (f x)
     (for-each f rest))))

(define (concat xss) (foldr append nil xss))
(define (concat-map f xs) (concat (map f xs)))

(define (any? p xs)
  (match xs
    (nil false)
    ((cons x rest) (or (p x) (any? p rest)))))

(define (all? p xs)
  (match xs
    (nil true)
    ((cons x rest) (and (p x) (all? p rest)))))

(define (member? x xs) (any? (lambda (y) (= x y)) xs))

(define (find p xs)
  (match xs
    (nil none)
    ((cons x rest) (if (p x) (some x) (find p rest)))))

(define (nth xs n)
  (match xs
    (nil (error "nth: index out of range"))
    ((cons x rest) (if (= n 0) x (nth rest (- n 1))))))

(define (take n xs)
  (match xs
    (nil nil)
    ((cons x rest) (if (<= n 0) nil (cons x (take (- n 1) rest))))))

(define (drop n xs)
  (match xs
    (nil nil)
    ((cons _ rest) (if (<= n 0) xs (drop (- n 1) rest)))))

(define (zip xs ys)
  (match (pair xs ys)
    ((pair (cons x xrest) (cons y yrest)) (cons (pair x y) (zip xrest yrest)))
    (_ nil)))

;; The integers from low up to, but not including, high.
(define (range low high)
  (define (go n acc)
    (if (< n low) acc (go (- n 1) (cons n acc))))
  (go (- high 1) nil))

;; A stable merge sort. less decides the order.
(define (sort less xs)
  (define (merge a b)
    (match (pair a b)
      ((pair nil _) b)
      ((pair _ nil) a)
      ((pair (cons x xrest) (cons y yrest))
       (if (less y x)
           (cons y (merge a yrest))
           (cons x (merge xrest b))))))
  (let ((n (length xs)))
    (if (< n 2)
        xs
        (let ((half (quot n 2)))
          (merge (sort less (take half xs))
                 (sort less (drop half xs)))))))

;; Strings

(define (string-join separator strings)
  (match strings
    (nil "")
    ((cons first rest)
     (foldl (lambda (acc s) (string-append acc (string-append separator s))) first rest))))

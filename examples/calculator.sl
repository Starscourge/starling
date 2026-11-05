;; A small interpreter and symbolic algebra system: the classic showcase for algebraic data types
;; and pattern matching. Nested patterns make the simplification rules read like the math, and the
;; compiler rejects any `match` that forgets a case.

(type expr
  (num int)
  (var string)
  (add expr expr)
  (mul expr expr)
  (neg-of expr))

;; Evaluates an expression. A variable with no value makes the result `none`.
(define (eval e env)
  (define (combine op a b)
    (match (pair (eval a env) (eval b env))
      ((pair (some x) (some y)) (some (op x y)))
      (_ none)))
  (match e
    ((num n) (some n))
    ((var name) (lookup name env))
    ((add a b) (combine + a b))
    ((mul a b) (combine * a b))
    ((neg-of a) (map-option neg (eval a env)))))

(define (lookup name env)
  (map-option snd (find (lambda (binding) (= (fst binding) name)) env)))

;; The derivative with respect to a variable.
(define (derive e x)
  (match e
    ((num _) (num 0))
    ((var name) (if (= name x) (num 1) (num 0)))
    ((add a b) (add (derive a x) (derive b x)))
    ((mul a b) (add (mul (derive a x) b) (mul a (derive b x))))
    ((neg-of a) (neg-of (derive a x)))))

;; Simplifies from the bottom up, using nested patterns for the algebraic identities.
(define (simplify e)
  (match e
    ((add a b) (simplify-add (simplify a) (simplify b)))
    ((mul a b) (simplify-mul (simplify a) (simplify b)))
    ((neg-of a)
     (match (simplify a)
       ((num n) (num (neg n)))
       ((neg-of inner) inner)
       (other (neg-of other))))
    (_ e)))

(define (simplify-add a b)
  (match (pair a b)
    ((pair (num 0) other) other)
    ((pair other (num 0)) other)
    ((pair (num x) (num y)) (num (+ x y)))
    (_ (add a b))))

(define (simplify-mul a b)
  (match (pair a b)
    ((pair (num 0) _) (num 0))
    ((pair _ (num 0)) (num 0))
    ((pair (num 1) other) other)
    ((pair other (num 1)) other)
    ((pair (num x) (num y)) (num (* x y)))
    (_ (mul a b))))

(define (to-string e)
  (match e
    ((num n) (show n))
    ((var name) name)
    ((add a b) (string-join " " (list "(" (to-string a) "+" (to-string b) ")")))
    ((mul a b) (string-join " " (list "(" (to-string a) "*" (to-string b) ")")))
    ((neg-of a) (string-append "-" (to-string a)))))

;; x*x + 3*x + 2
(define polynomial
  (add (mul (var "x") (var "x"))
       (add (mul (num 3) (var "x"))
            (num 2))))

(define environment (list (pair "x" 5)))

(print (to-string polynomial))
(print (eval polynomial environment))
(print (eval polynomial nil))

(define slope (derive polynomial "x"))

(print (to-string slope))
(print (to-string (simplify slope)))
(print (eval (simplify slope) environment))

;; Arbitrary-precision integers carry through with no extra work.
(print (eval (mul (var "big") (var "big")) (list (pair "big" (pow 10 25)))))

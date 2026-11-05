# The Starling language

A program is a sequence of top-level forms. Comments run from `;` to the end of the line.

## Values and types

| Type | Examples | Notes |
|---|---|---|
| `int` | `42`, `-7`, `123456789012345678901234567890` | Arbitrary precision |
| `bool` | `true`, `false` | |
| `string` | `"hello\n"` | Escapes: `\n` `\t` `\r` `\\` `\"` |
| `unit` | `unit` | The result of `print` and similar |
| `(list a)` | `(list 1 2)`, `'(1 2)`, `nil`, `()` | All elements share one type |
| `(-> a b c)` | `(lambda (x y) x)` | A function from `a` and `b` to `c` |

A type variable is one lowercase letter plus optional digits: `a`, `b`, `t1`.

## Definitions

```lisp
(define pi-ish 3)
(define (square x) (* x x))

(define (hypotenuse-squared a b)
  ;; A body can start with local definitions, which can call each other.
  (define (plus-square total n) (+ total (square n)))
  (plus-square (square a) b))

(print (hypotenuse-squared 3 4))
```

Function definitions are visible throughout the file, regardless of order. Other definitions and
top-level expressions run in source order, and using a value before it is defined is a runtime
error.

Defining the same name twice in one file is an error, but a program can shadow a standard library
function or a built-in. A REPL session can also redefine a name, and earlier code keeps the
binding it was compiled with.

### Signatures

Types are inferred, so signatures are optional. A signature needs a definition of that name in the
same file. Its type variables have to be truly general: the signature below would be rejected if the
body only worked on integers.

```lisp
(sig first-of (-> a b a))
(define (first-of x y) x)

(print (first-of 1 "ignored"))
```

Signatures also allow polymorphic recursion, which inference alone can't accept:

```lisp
(type (nested a)
  (flat a)
  (deep (nested (list a))))

(sig depth (-> (nested a) int))
(define (depth x)
  (match x
    ((flat _) 0)
    ((deep inner) (+ 1 (depth inner)))))

(print (depth (deep (deep (flat '((1)))))))
```

## Data types

```lisp
(type color red green blue)

(type (shape a)
  (circle a)
  (rectangle a a))

;; A constructor with no fields can be written with or without parentheses.
(define c1 red)
(define c2 (green))

;; Constructors with fields can be used as functions.
(print (map circle '(1 2)))
```

The standard library defines `(option a)`, with `none` and `(some a)`, and `(pair a b)`, with
`(pair a b)`.

## Expressions

```lisp
;; Conditionals. The else clause in cond is required.
(define n 5)
(print (if (> n 0) "positive" "not positive"))
(print (cond ((< n 0) "negative")
             ((= n 0) "zero")
             (else "positive")))

;; and and or short-circuit, and take any number of arguments.
(print (and (> n 0) (< n 10)))
(print (or (< n 0) (> n 3)))

;; let binds in parallel, let* in sequence, and letrec binds functions that can call each other.
(print (let ((a 1) (b 2)) (+ a b)))
(print (let* ((a 1) (b (+ a 1))) (* a b)))
(print (letrec ((even (lambda (k) (if (= k 0) true (odd (- k 1)))))
                (odd (lambda (k) (if (= k 0) false (even (- k 1))))))
         (even 10)))

;; Functions and sequencing. begin runs each form and returns the last.
(print ((lambda (x y) (+ x y)) 1 2))
(print (begin (print "first") "second"))

;; List literals. Quoted data can hold numbers, strings, booleans and lists.
(print (list 1 (+ 1 1) 3))
(print '(1 2 3))
(print '("a" "b"))

;; An expression can state its type. Here, a stands for "some type".
(print (the int (+ 1 2)))
(print (the (list a) (list 1)))
```

Only `+` and `*` take any number of arguments, as in `(+ 1 2 3)`. Every other function has a fixed
number of parameters, and may have none: `(define (f) 1)` is called as `(f)`.

## Pattern matching

```lisp
(type (tree a)
  leaf
  (node (tree a) a (tree a)))

(define (describe t)
  (match t
    (leaf "empty")
    ((node leaf _ leaf) "one node")
    ((node _ 0 _) "starts at zero")
    ((node _ value _) (string-append "value " (show value)))))

(print (describe (node leaf 0 (node leaf 1 leaf))))

(define (sum-first-two xs)
  (match xs
    (() 0)
    ((list a) a)
    ((cons a (cons b _)) (+ a b))))

(print (sum-first-two '(1 2 3)))
```

| Pattern | Matches |
|---|---|
| `_` | Anything, without binding it |
| `x` | Anything, and binds it to `x` |
| `0`, `"text"` | That literal |
| `true`, `false`, `unit` | That value |
| `leaf`, `(leaf)` | A constructor with no fields |
| `(node l v r)` | A constructor, with a pattern for each field |
| `nil`, `()` | The empty list |
| `(cons h t)` | A non-empty list |
| `(list a b)` | A list of exactly that many elements |

Clauses are tried from top to bottom. A `match` that doesn't cover every case is a compile error,
and so is a clause that can never match. Ints and strings have too many values to list, so
matching on them needs a catch-all clause.

## Built-in functions

| Function | Type | Notes |
|---|---|---|
| `+` `-` `*` | `(-> int int int)` | `+` and `*` accept more than two arguments when called directly |
| `quot` `rem` | `(-> int int int)` | Rounds toward zero. Division by zero is a runtime error |
| `neg` | `(-> int int)` | |
| `<` `<=` `>` `>=` | `(-> int int bool)` | |
| `=` `!=` | `(-> a a bool)` | Structural. Comparing functions is a runtime error |
| `not` | `(-> bool bool)` | |
| `string-append` | `(-> string string string)` | |
| `string-length` | `(-> string int)` | In bytes |
| `substring` | `(-> string int int string)` | Takes the start and the length |
| `show` | `(-> a string)` | The text `print` would write |
| `print` | `(-> a unit)` | Writes a value and a newline |
| `error` | `(-> string a)` | Stops the program with a message and a stack trace |

## Standard library

Written in Starling, in [`lib/prelude.sl`](../lib/prelude.sl), and loaded before every program.

| Area | Functions |
|---|---|
| Functions | `id` `const` `compose` `flip` |
| Integers | `abs` `min` `max` `even?` `odd?` `gcd` `pow`, and `div` `mod`, which round toward negative infinity |
| Pairs | `fst` `snd` |
| Options | `some?` `with-default` `map-option` |
| Lists | `null?` `head` `tail` `length` `reverse` `append` `map` `filter` `foldl` `foldr` `for-each` `sum` `product` `concat` `concat-map` `any?` `all?` `member?` `find` `nth` `take` `drop` `zip` `range` `sort` |
| Strings | `string-join` |

`(sort less xs)` is a stable merge sort, and `(range low high)` includes `low` but not `high`.

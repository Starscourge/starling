# Starling

[![CI](https://github.com/Starscourge/starling/actions/workflows/ci.yml/badge.svg)](https://github.com/Starscourge/starling/actions/workflows/ci.yml)

A statically typed functional language with Lisp-style syntax, compiled to bytecode and run on
its own stack VM. It is written in OCaml, in about 4,000 lines.

```lisp
(type (tree a)
  leaf
  (node (tree a) a (tree a)))

;; No annotations needed. The inferred type is
;; (-> (-> a a bool) a (tree a) (tree a)), so this works for any element type.
(define (insert less x t)
  (match t
    (leaf (node leaf x leaf))
    ((node l v r)
     (cond ((less x v) (node (insert less x l) v r))
           ((less v x) (node l v (insert less x r)))
           (else t)))))

(define (factorial n)
  (if (= n 0) 1 (* n (factorial (- n 1)))))

(print (factorial 30))
```

- **Types without annotations.** Hindley-Milner inference finds the most general type of every
  definition. Top-level definitions are grouped by dependency, so a function is only
  monomorphic among the functions it is actually mutually recursive with. Optional signatures
  are checked against the inferred type and allow polymorphic recursion.
- **Matches are checked.** A `match` that misses a case is a compile error that shows a value
  it doesn't handle, even when the gap is nested, and so is a clause that can never match.
- **Recursion just works.** The VM keeps its own stack rather than using the native one.
  100 million tail calls run in 6 MB of memory, 3 million non-tail calls work fine, and runaway
  recursion is an ordinary runtime error with a stack trace.
- **Integers don't overflow.** They stay native machine integers until they would overflow, then
  move to a hand-written big integer library with Karatsuba multiplication and Knuth's
  Algorithm D.
- **Errors point at the problem.** Compile errors underline the source, and runtime errors come
  with a stack trace.
- **It is tested.** Over 120 tests run in CI on OCaml 4.14 and 5.2. All integer arithmetic is
  tested against Zarith on operands chosen around each fast path's limits.

## Quickstart

You need [opam](https://opam.ocaml.org/doc/Install.html) and OCaml 4.14 or newer.

```bash
git clone https://github.com/Starscourge/starling.git
cd starling
opam install . --deps-only --with-test
dune build
dune test

dune exec starling -- run examples/calculator.sl
dune exec starling -- repl
```

The [`examples`](examples) folder has a symbolic algebra system, a polymorphic search tree,
an N-queens solver, and big-number arithmetic.

### A REPL session

```text
Starling 0.1.0. Type :help for help and :quit to leave.
> (define (twice f x) (f (f x)))
twice : (-> (-> a a) a a)
> (twice (lambda (n) (* n n)) 7)
2401 : int
> (pow 2 100)
1267650600228229401496703205376 : int
> (map show '(1 2 3))
("1" "2" "3") : (list string)
> :type foldl
(-> (-> a b a) a (list b) a)
```

### Errors

A type error:

```text
error: type mismatch: expected int, found string
 --> errors/type_mismatch.sl:2:16
  |
2 | (print (double "two"))
  |                ^^^^^
```

A `match` with a missing case, nested inside a pair:

```text
error: this match does not cover every case
 --> errors/missing_nested_case.sl:2:3
  |
2 |   (match (pair a b)
  |   ^^^^^^^^^^^^^^^^^
note: a value not handled: (pair (some _) none)
```

A signature promising more than the code delivers:

```text
error: type mismatch: expected (-> a a), found (-> int int)
 --> errors/signature_too_general.sl:2:1
  |
2 | (define (identity x) (+ x 1))
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
note: 'a' comes from a type signature, so the definition must work for every type
```

A runtime error:

```text
runtime error: division by zero
  in average at errors/division_by_zero.sl:2:3
  in report at errors/division_by_zero.sl:5:10
  in <main> at errors/division_by_zero.sl:8:1
```

### Looking inside

`starling check` prints the type of each definition, and `starling disasm` shows the bytecode.
This is a tail-recursive sum:

```lisp
(define (sum-to n acc)
  (if (= n 0)
      acc
      (sum-to (- n 1) (+ acc n))))
```

```text
sum-to (arity 2, locals 2, frame 6)
     0  JumpUnlessLocalInt local 0 = 0, 3
     1  GetLocal 1
     2  Return
     3  GetGlobal 44 (sum-to)
     4  SubLocalInt local 0, 1
     5  GetLocal 1
     6  GetLocal 0
     7  Add
     8  TailCall 2
```

## How it works

Each phase is a separate module with its own interface:

| Phase | Module | What it does |
|---|---|---|
| Read | [`lexer`](lib/lexer.ml), [`reader`](lib/reader.ml) | Text to S-expressions, keeping every source position |
| Parse | [`parse`](lib/parse.ml) | Checks the shape of each form and expands sugar like `cond` |
| Type check | [`infer`](lib/infer.ml), [`exhaustive`](lib/exhaustive.ml) | Inference with level-based generalization, and match checking with Maranget's algorithm |
| Lower | [`lower`](lib/lower.ml) | Resolves names and compiles pattern matching into simple tests |
| Generate | [`compile`](lib/compile.ml) | Closure conversion and bytecode, with every function's stack use verified |
| Run | [`vm`](lib/vm.ml), [`value`](lib/value.ml), [`bigint`](lib/bigint.ml) | The dispatch loop, integer promotion, and big integers |

[`docs/design.md`](docs/design.md) explains how each phase works and why it was built that way.
[`docs/language.md`](docs/language.md) is the language reference.

## Performance

Medians from `dune exec --profile release bench/bench.exe`, on an Intel Core Ultra 7 155H under
WSL2, with OCaml 4.14 and no flambda:

| Program | Time |
|---|---|
| `fib 32`, about 7 million calls | 211 ms |
| Tail-recursive loop, 10 million iterations | 310 ms |
| `map`, `filter` and `sum` over 1 million elements | 377 ms |
| Sort 200,000 integers | 936 ms |
| 9-queens, all solutions | 59 ms |

For scale, the same `fib 32` takes 157 ms on CPython 3.12 ([`bench/fib.py`](bench/fib.py)), so
Starling is about 1.3 times slower on call-heavy code. The main cost is OCaml's GC write barrier,
which runs on every write to the VM's stack, and [`docs/design.md`](docs/design.md#performance)
has the measurements behind that.

The big integer library gives up raw speed for simplicity. Zarith is backed by GMP, a heavily
tuned library, and is far faster at multiplication and division. The decimal representation does
win at printing:

| Operation | Bigint | Zarith |
|---|---|---|
| 2000! by repeated multiplication | 2.8 ms | 0.2 ms |
| Square a 77,000-digit number | 32 ms | 0.4 ms |
| Divide 154,000 digits by 77,000 digits | 498 ms | 1.1 ms |
| Print a 154,000-digit number | 1.5 ms | 3.9 ms |

## Limitations

This is a small language, and it leaves out plenty on purpose:

- There are no modules or imports, so a program is one file plus the standard library.
- There are no type classes. `<` only works on integers, and `=` and `print` are built in at
  every type.
- There's no mutable state, and functions aren't curried: `(f 1)` on a two-argument function is
  a type error rather than a partial application.
- Memory is managed by OCaml's garbage collector, not a custom one.
- Strings are byte strings, so `string-length` counts bytes rather than characters.

## License

[MIT](LICENSE)

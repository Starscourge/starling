# How Starling works

A tour of the implementation, in the order a program passes through it, with the reasoning behind
the main decisions.

```text
source text
   │  lexer, reader    S-expressions with source positions
   │  parse            surface AST, with sugar expanded
   │  infer            typed AST, every name resolved   (exhaustive checks each match)
   │  lower            core IR: unique variables, explicit tests
   │  compile          bytecode, one code object per function
   ▼  vm               a value
```

## Reading: `lexer.ml`, `reader.ml`

The lexer and reader are hand-written. S-expressions are simple enough that a generator would add
more than it saves, and writing them directly makes two things easy:

- **Every node keeps its source span.** That is what lets later phases underline the right text.
- **The REPL can tell incomplete input from invalid input.** The lexer and reader raise a separate
  `Incomplete` exception when input ends inside a list or a string, and `Reader.is_complete` uses
  it to decide whether to prompt for another line. A stray `)` still reports immediately.

## Parsing: `parse.ml`

The parser checks the shape of each special form and removes the sugar that needs no scope or type
information. `cond` becomes nested `if`s, `and` and `or` become `if`s that short-circuit, `let*`
becomes nested `let`s, and `(+ a b c)` folds to the left. Later phases get a smaller language to
handle, which matters most for type checking, where every construct is another case to get right.

Local `define`s become `letrec` (a run of function definitions, so they can call each other) or
`let` (a value definition).

Type variables must look like `a` or `t1`. This means a misspelled type such as `itn` is reported
as unknown, rather than silently becoming a fresh variable.

## Type inference: `infer.ml`

Inference is Hindley-Milner: walk the program, give unknown types a fresh variable, and unify
whenever two types have to agree. The interesting part is generalization, the step that makes
`let`-bound functions polymorphic.

### Generalization with levels

`(let ((id (lambda (x) x))) ...)` should give `id` the type "for all a, a to a". But in this
function, `y` must not be polymorphic, because its type is tied to `x`:

```lisp
(define (f x)
  (let ((y x))
    y))
```

The textbook rule is to generalize only variables that don't appear in the environment, which means
scanning the environment at every `let`. Levels make this cheap, following Rémy's approach, also
used by OCaml's own type checker:

1. A global counter tracks the current `let` depth, and each new type variable records it.
2. Entering a `let`'s right-hand side raises the counter, and leaving lowers it.
3. A variable whose level is deeper than the current depth wasn't reachable from outside, so it can
   be generalized.

The subtle part is what happens during unification. Consider:

```lisp
(define (g x)
  (let ((f (lambda (y) (begin (= x y) y))))
    f))
```

Here `y` is created inside the `let`, so its level looks generalizable. But `(= x y)` makes `x` and
`y` the same type, so `y` is now reachable from the enclosing scope. To handle this, `bind` lowers
the level of every variable in a type when binding it to a shallower variable. Without that step,
`f` would wrongly generalize, and the program could use `x` at two different types.

The same pass does the occurs check, which rejects infinite types like the one in
`(lambda (x) (x x))`.

Starling has no mutable references, so it needs no value restriction: every `let` can generalize.

### Top-level definitions

Definitions in a file can refer to each other in any order, so they are checked in dependency order,
using Tarjan's algorithm to find strongly connected components. This matters for precision. If
`a` calls `b`, but `b` doesn't call `a`, then `b` gets fully generalized before `a` is checked, and
`a` can use it at several types. A function is only monomorphic among the functions it is actually
mutually recursive with.

A definition with a signature is a special case. Anything that uses it can rely on its declared type
before its body is checked, so it creates no ordering constraint. That is what allows polymorphic
recursion, where a function calls itself at a different type. Its body is checked with the signature's
variables replaced by *rigid* variables, which unify only with themselves, so a body that isn't as
general as promised gets rejected.

### Elaboration

Inference also produces a new tree, `Typed`, with names resolved. For instance, a bare identifier
becomes either a variable or a specific constructor. Later phases then don't have to repeat that
scoping logic.

## Checking matches: `exhaustive.ml`

This uses the usefulness algorithm from Maranget's "Warnings for pattern matching". Treat the clauses
as a matrix with a row per clause. A pattern is *useful* against some rows if a value exists that
matches it and none of the rows. Both checks follow from that one idea:

- A clause is unreachable if it isn't useful against the clauses above it.
- A match is exhaustive if an all-wildcard row isn't useful against all of them.

The algorithm works by looking at the first column and either specializing the matrix to one
constructor or taking the rows with a wildcard there. A variant of it returns an example value rather
than a yes or no, which produces messages like `a value not handled: (pair (some _) none)`.

## Lowering: `lower.ml`, `core.ml`

Lowering produces the core IR, where every variable is unique and pattern matching has become
explicit tests.

Matching follows the approach in chapter 5 of Peyton Jones's *The Implementation of Functional
Programming Languages*: take the first column of the clause matrix, split the rows into runs with the
same kind of pattern, compile each run, and chain the runs together.

Decision trees are the other common approach, but they can duplicate clause bodies. Starling instead
uses `Fatbar (a, b)` and `Fail`, meaning "run `a`, and if it reaches `Fail`, run `b` instead". Code
stays linear in the size of the patterns. The cost is that some tests can repeat after a fall-through,
which seemed the right trade for a small interpreter.

Function definitions are moved to the start of a unit, which is what makes them visible throughout a
file. They only allocate a closure, so moving them has no visible effect.

## Code generation: `compile.ml`

### Closures

Each lambda becomes its own code object, and its free variables get copied into the closure when it
is created. These are called flat closures. Copying is safe because Starling values never change, so a
captured copy can't go out of date.

`letrec` is the one exception, because mutually recursive closures have to refer to each other. It
creates all the closures first, then fills in the references with `PatchCapture`.

### Tail calls

`expr` takes a `~tail` flag. In tail position, a call becomes `TailCall` rather than `Call`, and the
flag passes down through `if`, `let`, `begin` and `match` so that every tail position is found.

### The stack checker

`Bytecode.max_operand_depth` walks every path through each function, and rejects code that would
underflow the stack, or where two paths reach the same instruction with different stack depths. It also
calculates how much stack the function needs.

The VM relies on that number to avoid checking the stack on every push. A code generation bug is
therefore caught at compile time, instead of corrupting the VM's memory while a program runs.

## The VM: `vm.ml`

### One explicit stack

Calls don't use the native OCaml stack. Each frame lives on one array:

```text
stack.(fp - 1)   the running closure
stack.(fp + i)   local slot i, with parameters first
above those      the operand stack, up to sp
```

This has a few effects:

- Recursion depth is limited by a configured limit (16 million slots), rather than by the system stack.
- Hitting the limit is an ordinary runtime error that can report a stack trace.
- A tail call slides the new arguments down over the current frame and jumps, so tail recursion runs in
  constant space. 100 million tail calls peak at 6 MB of memory.

A saved frame is just two integers, the caller's `pc` and `fp`. The caller's code comes back from its
closure at `stack.(fp - 1)`, so a call writes no pointers to the frame array, avoiding the write barrier
described below.

The interpreter is a single function, and no closure captures its registers. This lets OCaml treat `sp`,
`fp` and `pc` as local variables rather than heap-allocated references.

### Integers

There are two representations. `Int` holds anything that fits in a native `int`, and `Big` holds
everything else. Arithmetic runs on `Int` with an overflow check and falls back to `Bigint` when needed.

Results that fit always go back to `Int`, so a number has exactly one representation. That keeps equality
simple, since an `Int` and a `Big` can never be equal.

The overflow checks rely on two's complement: an addition overflowed exactly when both operands' signs
differ from the result's sign.

## Big integers: `bigint.ml`

Numbers are a sign and an array of base 10⁹ limbs, least significant first.

- **Base 10⁹ rather than a power of two.** Printing takes linear time, because each limb is already nine
  decimal digits. The product of two limbs still fits in OCaml's 63-bit integers, with room for carries.
- **Karatsuba multiplication** above 32 limbs. It computes the middle term from one extra multiplication
  instead of two, giving O(n^1.585).
- **Knuth's Algorithm D** for division. It estimates each quotient limb from the top limbs of the operands,
  corrects the estimate, and in rare cases adds the divisor back. Normalizing the divisor first keeps
  the estimate within 2 of the correct value.

## Performance

Without a profiler available, the main cost was found by experiment. Removing bounds checks made no
measurable difference, and a larger minor heap didn't help either. A microbenchmark then showed the cost
of one write to the stack array:

| Write | Cost |
|---|---|
| Array of `value`, with the GC's write barrier | about 1.4 ns |
| Array of `int`, with no barrier | about 0.3 ns |

The VM's stack is a large array in the major heap, so every push goes through `caml_modify`. Representing
integers without boxing, using `Obj.magic`, would only have saved about 0.3 ns per write, which didn't
justify giving up type safety.

So the useful optimization was fewer writes. Instructions like `SubLocalInt` and `JumpUnlessLocalInt` do the
work of three or four plain instructions, reading a local directly. That took `fib 32` from 385 ms to
207 ms and a 10-million-iteration loop from 535 ms to 307 ms.

A substantially faster VM would need a stack the GC doesn't treat as an ordinary heap object, which means C
stubs and a custom root-scanning hook. That was out of scope here.

## Testing

| Kind | Where | What it covers |
|---|---|---|
| Unit tests | `test/test_*.ml` | Each phase, including inputs that must be rejected |
| Differential | `test_bigint.ml`, `test_vm.ml` | All integer operations against Zarith |
| Golden | `examples/`, `test/errors/` | Exact program output and exact error messages |
| End to end | `test/cli.t` | The command line tool and REPL, as a user runs them |

The differential tests use generators aimed at the risky inputs. Big integer operands lean toward long runs
of 0s and 9s, which produce the carries, borrows and quotient overestimates that random digits almost never
reach. VM operands cluster around 2³¹, 2⁴⁰ and the limits of a native `int`, which is where each fast path
stops being valid.

Test strength was checked by injecting bugs by hand: removing overflow checks, skipping level lowering,
disabling closure patching, and so on. That found real gaps. The first version of the level test never
exercised lowering, because unifying in the other direction is safe anyway, and nothing tested multiplication
near its fast-path limit. Both now have tests that fail when the code is broken.

## Not built

A few natural extensions, roughly in order of interest:

- **Lightweight processes.** The dispatch loop could switch between green threads every N instructions, as
  the BEAM VM does, giving Erlang-style concurrency with message passing.
- **Modules and imports.**
- **Type classes**, which would let `<` work on more than integers, and replace the special cases for `=` and
  `print`.
- **Macros.** The syntax is ready for them, but they would have to run before type checking.

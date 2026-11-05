The command line tool, tested end to end.

  $ cat > square.sl <<'EOF'
  > (define (square x) (* x x))
  > (define (twice f x) (f (f x)))
  > (print (twice square 3))
  > EOF

Running a program:

  $ starling run square.sl
  81

Type-checking prints each definition's inferred type, and doesn't run anything:

  $ starling check square.sl
  square : (-> int int)
  twice : (-> (-> a a) a a)

Disassembly shows the bytecode, also without running anything:

  $ starling disasm square.sl
  <main> (arity 0, locals 0, frame 3)
       0  MakeClosure square []
       1  SetGlobal 44
       2  Push unit
       3  Pop
       4  MakeClosure twice []
       5  SetGlobal 45
       6  Push unit
       7  Pop
       8  GetGlobal 45 (twice)
       9  GetGlobal 44 (square)
      10  Push 3
      11  Call 2
      12  Print
      13  Return
  
  square (arity 1, locals 1, frame 3)
       0  GetLocal 0
       1  GetLocal 0
       2  Mul
       3  Return
  
  twice (arity 2, locals 2, frame 5)
       0  GetLocal 0
       1  GetLocal 0
       2  GetLocal 1
       3  Call 1
       4  TailCall 1
  

A compile error exits with 1 and points at the source:

  $ cat > bad.sl <<'EOF'
  > (print (+ 1 "two"))
  > EOF
  $ starling run bad.sl
  error: type mismatch: expected int, found string
   --> bad.sl:1:13
    |
  1 | (print (+ 1 "two"))
    |             ^^^^^
  [1]

A runtime error also exits with 1, and output printed before it still appears:

  $ cat > crash.sl <<'EOF'
  > (print "before")
  > (print (quot 1 0))
  > EOF
  $ starling run crash.sl
  before
  runtime error: division by zero
    in <main> at crash.sl:2:8
  [1]

Bad usage and missing files exit with 2:

  $ starling
  Starling: a statically typed functional language.
  
  Usage:
    starling run FILE      compile and run a program
    starling check FILE    type-check a program and print each definition's type
    starling disasm FILE   print a program's bytecode without running it
    starling repl          start an interactive session
    starling --version     print the version
  [2]

  $ starling run missing.sl
  starling: missing.sl: No such file or directory
  [2]

  $ starling --version
  0.1.0

The REPL shows results with their types, keeps definitions between inputs, waits for more input when a
form is unfinished, and carries on after errors:

  $ cat > session.txt <<'EOF'
  > (+ 1 2)
  > (define (inc x)
  >   (+ x 1))
  > (inc 41)
  > "text"
  > (print "side effect")
  > :type map
  > :type inc
  > (+ 1 "oops")
  > (quot 1 0)
  > (inc 1)
  > :disasm 1
  > :nonsense
  > :help
  > :quit
  > (print "not reached")
  > EOF
  $ starling repl < session.txt 2>&1
  Starling 0.1.0. Type :help for help and :quit to leave.
  > 3 : int
  > ... inc : (-> int int)
  > 42 : int
  > "text" : string
  > side effect
  > (-> (-> a b) (list a) (list b))
  > (-> int int)
  > error: type mismatch: expected int, found string
   --> <repl>:1:6
    |
  1 | (+ 1 "oops")
    |      ^^^^^^
  > runtime error: division by zero
    in <main> at <repl>:1:1
  > 2 : int
  > <main> (arity 0, locals 0, frame 1)
       0  Push 1
       1  Return
  
  > unknown command :nonsense (try :help)
  > Enter an expression or definition. Input can span several lines.
  
    :type EXPR     show the type of an expression without running it
    :disasm EXPR   show the bytecode for an expression
    :help          show this message
    :quit          leave (Ctrl-D also works)
  > 

End of input leaves cleanly too:

  $ printf '(+ 1 1)\n' | starling repl
  Starling 0.1.0. Type :help for help and :quit to leave.
  > 2 : int
  > 

open Starling

let check_source text =
  let source : Diagnostic.source = { name = "test.sl"; text } in
  Infer.check_program Infer.initial_env (Parse.program (Reader.read source))

(* The inferred type of [name] after checking the program. *)
let type_of name text =
  let result = check_source text in
  match List.assoc_opt name result.definitions with
  | Some scheme -> Types.scheme_to_string scheme
  | None -> Alcotest.failf "no definition named %s" name

let has_type name expected text =
  Alcotest.(check string) text expected (type_of name text)

let contains ~fragment text =
  let n = String.length fragment in
  let rec go i =
    i + n <= String.length text && (String.sub text i n = fragment || go (i + 1))
  in
  go 0

let accepted text = ignore (check_source text)

let rejected fragment text =
  match check_source text with
  | _ -> Alcotest.failf "expected a rejection containing %S for: %s" fragment text
  | exception Diagnostic.Error { message; notes; _ } ->
      let full = String.concat "\n" (message :: notes) in
      if not (contains ~fragment full) then
        Alcotest.failf "for %s\nexpected an error containing %S, got %S" text fragment
          full

let test_inference () =
  has_type "x" "int" "(define x 1)";
  has_type "s" "string" "(define s \"hi\")";
  has_type "b" "bool" "(define b true)";
  has_type "u" "unit" "(define u unit)";
  has_type "f" "(-> int int)" "(define (f x) (+ x 1))";
  has_type "id" "(-> a a)" "(define (id x) x)";
  has_type "const" "(-> a b a)" "(define (const x y) x)";
  has_type "thunk" "(-> int)" "(define (thunk) 1)";
  has_type "compose" "(-> (-> a b) (-> c a) c b)" "(define (compose f g x) (f (g x)))";
  has_type "apply-twice" "(-> (-> a a) a a)" "(define (apply-twice f x) (f (f x)))";
  has_type "e" "a" "(define e (error \"boom\"))"

let test_lists () =
  has_type "xs" "(list int)" "(define xs (list 1 2 3))";
  has_type "q" "(list int)" "(define q '(1 2 3))";
  has_type "nested" "(list (list int))" "(define nested '((1) (2 3)))";
  has_type "empty" "(list a)" "(define empty nil)";
  has_type "empty" "(list a)" "(define empty ())";
  has_type "c" "(list string)" "(define c (cons \"a\" nil))";
  has_type "map" "(-> (-> a b) (list a) (list b))"
    "(define (map f xs)\n\
    \   (match xs\n\
    \     (nil nil)\n\
    \     ((cons h t) (cons (f h) (map f t)))))";
  rejected "expected int, found string" "(list 1 \"two\")";
  rejected "expected int, found string" "'(1 \"two\")"

(* = works at any type, but both sides must have the same one. *)
let test_equality_is_polymorphic () =
  accepted "(= 1 2)";
  accepted "(= \"a\" \"b\")";
  accepted "(= (list 1) (list 2))";
  accepted "(!= true false)";
  rejected "type mismatch" "(= 1 \"one\")"

let test_let_polymorphism () =
  accepted "(let ((id (lambda (x) x))) (begin (id 1) (id \"s\") (id true)))";
  accepted "(define (id x) x) (id 1) (id \"s\")";
  (* Lambda parameters are monomorphic: only let generalizes. *)
  rejected "type mismatch" "(lambda (f) (begin (f 1) (f \"s\")))";
  has_type "f" "(-> a a)" "(define (f x) (let ((y x)) y))"

(* Each of these is unsound if it type-checks: x would have two different types. *)
let test_levels_prevent_unsound_generalization () =
  (* Generalizing a variable that belongs to the enclosing lambda. *)
  rejected "type mismatch" "(lambda (x) (let ((y x)) (begin (+ y 1) (string-length y))))";
  rejected "type mismatch"
    "(lambda (x) (let ((f (lambda (z) x))) (begin (+ (f 1) 1) (string-length (f 2)))))";
  rejected "type mismatch"
    "(lambda (x)\n   (let ((y (let ((z x)) z)))\n     (begin (+ y 1) (string-length y))))";
  (* Here y is created inside the let, but unifying x with y binds the outer x to the inner y.
     That makes y reachable from the enclosing scope, so y must not generalize. Only lowering
     y's level during unification catches this: the variable's own level looks generalizable,
     and x and y have become the same variable. *)
  rejected "type mismatch"
    "(lambda (x)\n\
    \   (let ((f (lambda (y) (begin (= x y) y))))\n\
    \     (begin (+ (f 1) 1) (string-length (f \"s\")))))"

let test_type_errors () =
  rejected "expected int, found string" "(+ 1 \"two\")";
  rejected "expected bool, found int" "(if 1 2 3)";
  rejected "expected int, found string" "(if true 1 \"x\")";
  rejected "this function takes 2 argument(s), but got 3" "(+ 1 2 3 4 5) (- 1 2 3)";
  rejected "this function takes 1 argument(s), but got 0" "(define (f x) x) (f)";
  rejected "this is not a function" "(1 2)";
  rejected "this is not a function" "(\"f\" 2)";
  rejected "unbound variable 'nope'" "nope";
  rejected "infinite type" "(lambda (x) (x x))";
  rejected "infinite type" "(define (f x) (f (list x)) (list (f x)))";
  rejected "expected int, found string" "(the int \"s\")"

let test_suggestions () =
  rejected "did you mean 'length'?" "(define (length xs) 0) (lenght nil)";
  rejected "did you mean 'print'?" "(prnt 1)"

let test_recursion () =
  has_type "fact" "(-> int int)" "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))";
  has_type "go" "(-> int int)"
    "(define (f) (letrec ((inner (lambda (n) (inner n)))) (inner 1)))\n\
     (define (go n) (if (< n 0) n (go (- n 1))))";
  accepted "(define (f x) (letrec ((a (lambda (n) (b n))) (b (lambda (n) (a n)))) (a x)))"

let test_mutual_recursion () =
  let program =
    "(define (even? n) (if (= n 0) true (odd? (- n 1))))\n\
     (define (odd? n) (if (= n 0) false (even? (- n 1))))"
  in
  has_type "even?" "(-> int bool)" program;
  has_type "odd?" "(-> int bool)" program

(* Dependency analysis: [a] calls [b], which is defined later and isn't part of a's cycle, so
   [b] must be generalized before [a] is checked. *)
let test_definition_order_and_sccs () =
  let program = "(define (a x) (b x))\n(define (b y) y)\n(a 1)\n(a \"s\")" in
  has_type "a" "(-> a a)" program;
  has_type "b" "(-> a a)" program;
  (* Top-level expressions see definitions that come later in the file. *)
  accepted "(print (f 1)) (define (f x) x)"

let test_data_types () =
  let tree = "(type (tree a) (leaf) (node (tree a) a (tree a)))\n" in
  has_type "t" "(tree int)" (tree ^ "(define t (node (leaf) 1 (leaf)))");
  has_type "e" "(tree a)" (tree ^ "(define e (leaf))");
  has_type "bare" "(tree a)" (tree ^ "(define bare leaf)");
  has_type "mk" "(-> (tree a) a (tree a) (tree a))" (tree ^ "(define mk node)");
  has_type "insert" "(-> int (tree int) (tree int))"
    (tree
   ^ "(define (insert x t)\n\
     \   (match t\n\
     \     ((leaf) (node (leaf) x (leaf)))\n\
     \     ((node l v r)\n\
     \      (cond ((< x v) (node (insert x l) v r))\n\
     \            ((> x v) (node l v (insert x r)))\n\
     \            (else t)))))");
  rejected "expected (tree int), found string"
    (tree ^ "(define t (node (leaf) 1 \"oops\"))");
  rejected "constructor 'node' takes 3 argument(s), but got 1" (tree ^ "(node 1)");
  rejected "constructor 'node' takes 3 argument(s), but this pattern has 1"
    (tree ^ "(lambda (t) (match t ((node x) 1) ((leaf) 2)))");
  rejected "unknown constructor 'nope'" "(lambda (t) (match t ((nope) 1)))"

let test_type_declaration_errors () =
  rejected "unknown type 'itn'" "(type box (box itn))";
  rejected "'int' is a built-in type" "(type int (foo))";
  rejected "type 'list' takes 1 argument(s), but got 0" "(type box (box list))";
  rejected "type 'box' takes 1 argument(s), but got 2"
    "(type (box a) (mk a)) (type other (other (box int int)))";
  rejected "type variable 'b' is not a parameter" "(type (box a) (mk b))";
  rejected "type 'box' is defined twice" "(type box (x)) (type box (y))";
  rejected "constructor 'x' is defined twice" "(type box (x)) (type crate (x))";
  (* Types in the same unit can refer to each other in either order. *)
  accepted "(type forest (none) (trees rose forest)) (type rose (rose int forest))"

let test_signatures () =
  has_type "f" "(-> int int)" "(sig f (-> int int)) (define (f x) x)";
  has_type "id" "(-> a a)" "(sig id (-> a a)) (define (id x) x)";
  rejected "comes from a type signature" "(sig bad (-> a a)) (define (bad x) (+ x 1))";
  rejected "comes from a type signature" "(sig swap (-> a b a)) (define (swap x y) y)";
  rejected "expected (-> int string), found (-> int int)"
    "(sig f (-> int string)) (define (f x) (+ x 1))";
  rejected "has no definition" "(sig f (-> int int))";
  rejected "more than one signature"
    "(sig f (-> int int)) (sig f (-> int int)) (define (f x) x)";
  rejected "unknown type 'itn'" "(sig f (-> itn int)) (define (f x) x)";
  (* Polymorphic recursion needs the signature: this calls itself at a bigger type each time,
     which plain inference cannot accept. *)
  let nested_body =
    "(define (depth x)\n\
    \   (match x\n\
    \     ((flat _) 0)\n\
    \     ((deep inner) (+ 1 (depth inner)))))"
  in
  let nested = "(type (nested a) (flat a) (deep (nested (list a))))\n" in
  has_type "depth" "(-> (nested a) int)"
    (nested ^ "(sig depth (-> (nested a) int))\n" ^ nested_body);
  rejected "infinite type" (nested ^ nested_body);
  (* Annotations on expressions use flexible variables. *)
  has_type "xs" "(list int)" "(define xs (the (list a) (list 1)))"

let test_shadowing () =
  has_type "f" "(-> string string)" "(define (f x) (let ((x (string-append x \"!\"))) x))";
  (* A local variable can reuse a constructor's name in expressions. *)
  accepted "(type (pair a b) (pair a b)) (define (f pair) (+ pair 1)) (f 1)";
  (* User definitions can replace built-ins. *)
  has_type "+" "(-> a a)" "(define (+ x) x)"

let test_repeated_definitions () =
  rejected "'x' is defined twice" "(define x 1) (define x 2)"

(* Exhaustiveness *)

let color = "(type color red green blue)\n"
let tree = "(type (tree a) (leaf) (node (tree a) a (tree a)))\n"

let test_exhaustive_matches_accepted () =
  accepted (color ^ "(lambda (c) (match c (red 1) (green 2) (blue 3)))");
  accepted (color ^ "(lambda (c) (match c (red 1) (_ 2)))");
  accepted (color ^ "(lambda (c) (match c (other other)))");
  accepted "(lambda (b) (match b (true 1) (false 2)))";
  accepted "(lambda (u) (match u (unit 1)))";
  accepted "(lambda (xs) (match xs (nil 0) ((cons _ _) 1)))";
  accepted
    "(lambda (xs) (match xs ((list) 0) ((list a) a) ((cons a (cons b _)) (+ a b))))";
  accepted "(lambda (n) (match n (0 1) (1 2) (_ 3)))";
  accepted "(lambda (s) (match s (\"a\" 1) (_ 2)))";
  accepted
    (tree ^ "(lambda (t) (match t ((leaf) 0) ((node (leaf) v _) v) ((node _ v _) v)))")

let test_missing_cases () =
  rejected "does not cover every case" (color ^ "(lambda (c) (match c (red 1)))");
  rejected "a value not handled: green" (color ^ "(lambda (c) (match c (red 1) (blue 2)))");
  rejected "a value not handled: false" "(lambda (b) (match b (true 1)))";
  rejected "a value not handled: nil" "(lambda (xs) (match xs ((cons _ _) 1)))";
  rejected "a value not handled: (cons _ _)" "(lambda (xs) (match xs (nil 1)))";
  (* Infinite types need a catch-all. *)
  rejected "a value not handled: _" "(lambda (n) (match n (0 1) (1 2)))";
  rejected "a value not handled: _" "(lambda (s) (match s (\"a\" 1)))";
  (* The missing case can be nested. *)
  rejected "a value not handled: (cons _ nil)"
    "(lambda (xs) (match xs (nil 0) ((cons _ (cons _ _)) 1)))";
  rejected "a value not handled: (node leaf _ (node _ _ _))"
    (tree
   ^ "(lambda (t)\n\
     \  (match t\n\
     \    ((leaf) 0)\n\
     \    ((node (node _ _ _) _ _) 1)\n\
     \    ((node (leaf) _ (leaf)) 2)))");
  rejected "a value not handled: (cons true _)"
    "(lambda (xs) (match xs (nil 0) ((cons false _) 1)))"

let test_unreachable_clauses () =
  let unreachable = "can never match" in
  rejected unreachable (color ^ "(lambda (c) (match c (_ 1) (red 2)))");
  rejected unreachable (color ^ "(lambda (c) (match c (x 1) (y 2)))");
  rejected unreachable (color ^ "(lambda (c) (match c (red 1) (red 2) (_ 3)))");
  rejected unreachable (color ^ "(lambda (c) (match c (red 1) (green 2) (blue 3) (_ 4)))");
  rejected unreachable "(lambda (n) (match n (1 1) (1 2) (_ 3)))";
  rejected unreachable "(lambda (xs) (match xs ((cons _ _) 1) (nil 2) ((cons 1 _) 3)))"

let test_pattern_errors () =
  rejected "bound more than once" "(lambda (xs) (match xs ((cons a (cons a _)) 1) (_ 2)))";
  rejected "expected int, found string"
    "(lambda (n) (begin (+ n 1) (match n (\"s\" 1) (_ 2))))";
  rejected "expected (list int), found color"
    (color
   ^ "(lambda (xs) (begin (+ 1 (match xs ((cons h _) h) (nil 0))) (match xs (red 1))))")

let test_match_binds_pattern_types () =
  has_type "first" "(-> (list a) a a)"
    "(define (first xs default) (match xs ((cons h _) h) (nil default)))";
  (* What one match learns about xs carries over to the next. *)
  rejected "expected string, found int"
    "(lambda (xs)\n\
    \   (begin\n\
    \     (string-length (match xs ((cons h _) h) (nil \"\")))\n\
    \     (match xs ((cons 1 _) 1) (_ 2))))";
  rejected "expected int, found string" "(+ (match 1 (x \"s\")) 1)"

let test_type_of_expr () =
  let parse text =
    let source : Diagnostic.source = { name = "test.sl"; text } in
    Parse.expr (List.hd (Reader.read source))
  in
  let check text expected =
    Alcotest.(check string)
      text expected
      (Types.scheme_to_string (Infer.type_of_expr Infer.initial_env (parse text)))
  in
  check "1" "int";
  check "(lambda (x) x)" "(-> a a)";
  check "(lambda (f xs) (f (cons 1 xs)))" "(-> (-> (list int) a) (list int) a)";
  check "+" "(-> int int int)";
  check "=" "(-> a a bool)";
  check "print" "(-> a unit)"

(* Errors must not leave the checker in a state that breaks later checks, because the REPL
   keeps going after a mistake. *)
let test_recovery_after_error () =
  (try ignore (check_source "(let ((x (+ 1 \"s\"))) x)") with Diagnostic.Error _ -> ());
  has_type "id" "(-> a a)" "(define (id x) x)"

let test_last_type () =
  let check text expected =
    let result = check_source text in
    Alcotest.(check (option string))
      text expected
      (Option.map Types.scheme_to_string result.last_type)
  in
  check "1" (Some "int");
  check "(define x 1)" None;
  check "1 (define x 1)" None;
  check "(define x 1) x" (Some "int");
  check "(type box (box int))" None

let suite =
  [
    Alcotest.test_case "inference" `Quick test_inference;
    Alcotest.test_case "lists" `Quick test_lists;
    Alcotest.test_case "equality is polymorphic" `Quick test_equality_is_polymorphic;
    Alcotest.test_case "let polymorphism" `Quick test_let_polymorphism;
    Alcotest.test_case "levels prevent unsound generalization" `Quick
      test_levels_prevent_unsound_generalization;
    Alcotest.test_case "type errors" `Quick test_type_errors;
    Alcotest.test_case "suggestions" `Quick test_suggestions;
    Alcotest.test_case "recursion" `Quick test_recursion;
    Alcotest.test_case "mutual recursion" `Quick test_mutual_recursion;
    Alcotest.test_case "definition order and SCCs" `Quick test_definition_order_and_sccs;
    Alcotest.test_case "data types" `Quick test_data_types;
    Alcotest.test_case "type declaration errors" `Quick test_type_declaration_errors;
    Alcotest.test_case "signatures" `Quick test_signatures;
    Alcotest.test_case "shadowing" `Quick test_shadowing;
    Alcotest.test_case "repeated definitions" `Quick test_repeated_definitions;
    Alcotest.test_case "exhaustive matches accepted" `Quick
      test_exhaustive_matches_accepted;
    Alcotest.test_case "missing cases" `Quick test_missing_cases;
    Alcotest.test_case "unreachable clauses" `Quick test_unreachable_clauses;
    Alcotest.test_case "pattern errors" `Quick test_pattern_errors;
    Alcotest.test_case "match binds pattern types" `Quick test_match_binds_pattern_types;
    Alcotest.test_case "type_of_expr" `Quick test_type_of_expr;
    Alcotest.test_case "recovery after error" `Quick test_recovery_after_error;
    Alcotest.test_case "last_type" `Quick test_last_type;
  ]

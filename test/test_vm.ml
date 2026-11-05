open Starling

(* Runs a program in a new session and returns everything it printed. *)
let run text =
  let buf = Buffer.create 64 in
  let session = Driver.create ~out:(Buffer.add_string buf) () in
  ignore (Driver.eval session { name = "test.sl"; text });
  Buffer.contents buf

let prints name expected text = Alcotest.(check string) name expected (run text)

(* Checks that the program's last expression has the given printed value. *)
let evaluates expected text =
  let session = Driver.create ~out:ignore () in
  let outcome = Driver.eval session { name = "test.sl"; text } in
  Alcotest.(check string) text expected (Value.repr outcome.value)

let contains ~fragment text =
  let n = String.length fragment in
  let rec go i =
    i + n <= String.length text && (String.sub text i n = fragment || go (i + 1))
  in
  go 0

let fails_at_runtime fragment text =
  match run text with
  | _ -> Alcotest.failf "expected a runtime error containing %S" fragment
  | exception Vm.Runtime_error { message; _ } ->
      if not (contains ~fragment message) then
        Alcotest.failf "expected a runtime error containing %S, got %S" fragment message

let test_basics () =
  evaluates "3" "(+ 1 2)";
  evaluates "6" "(+ 1 2 3)";
  evaluates "\"hi\"" "\"hi\"";
  evaluates "true" "(< 1 2)";
  evaluates "false" "(not true)";
  evaluates "unit" "unit";
  evaluates "unit" "(define x 1)";
  evaluates "1" "(if true 1 2)";
  evaluates "2" "(if false 1 2)";
  evaluates "3" "(let ((x 1) (y 2)) (+ x y))";
  evaluates "2" "(let* ((x 1) (y (+ x 1))) y)";
  evaluates "2" "(begin 1 2)";
  evaluates "\"z\"" "(cond (false \"x\") (false \"y\") (else \"z\"))";
  (* Parallel let: y sees the outer x. *)
  evaluates "1" "(define x 1) (let ((x 2) (y x)) y)";
  (* A binding's right-hand side sees the outer scope, not the binding itself. *)
  evaluates "2" "(define x 1) (let ((x (+ x 1))) x)";
  evaluates "3" "(let* ((x 1) (x (+ x 1)) (x (+ x 1))) x)"

let test_short_circuit () =
  evaluates "false" "(and false (error \"not reached\"))";
  evaluates "true" "(or true (error \"not reached\"))";
  evaluates "true" "(and)";
  evaluates "false" "(or)";
  evaluates "true" "(and true true true)";
  evaluates "false" "(or false false false)"

let test_printing_values () =
  prints "scalars" "1\nhi\ntrue\nunit\n"
    "(print 1) (print \"hi\") (print true) (print unit)";
  prints "lists" "(1 2 3)\n()\n((1) (2))\n"
    "(print '(1 2 3)) (print nil) (print '((1) (2)))";
  prints "strings in data keep quotes" "(\"a\" \"b\")\n" "(print '(\"a\" \"b\"))";
  prints "data" "none\n(some 1)\n(some (some \"x\"))\n"
    "(print none) (print (some 1)) (print (some (some \"x\")))";
  prints "functions" "<function id>\n<function <lambda>>\n"
    "(print id) (print (lambda (x) x))";
  evaluates "\"12\"" "(show 12)";
  evaluates "\"(1 2)\"" "(show '(1 2))";
  evaluates "\"a\"" "(show \"a\")"

let test_closures () =
  evaluates "15" "(define (adder n) (lambda (x) (+ x n))) ((adder 10) 5)";
  (* Each call gets its own captured value. *)
  evaluates "(11 21)"
    "(define (adder n) (lambda (x) (+ x n)))\n\
     (let ((a (adder 10)) (b (adder 20))) (list (a 1) (b 1)))";
  (* Captures pass through several levels of nesting. *)
  evaluates "6" "(define (f a) (lambda (b) (lambda (c) (+ a (+ b c))))) (((f 1) 2) 3)";
  (* Slots get reused, so a captured value must not change when one is. *)
  evaluates "(1 2)"
    "(define (make)\n\
    \  (let ((f (let ((x 1)) (lambda () x))))\n\
    \    (let ((y 2)) (list (f) y))))\n\
     (make)"

let test_local_recursion () =
  evaluates "120"
    "(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))";
  evaluates "true"
    "(letrec ((even (lambda (n) (if (= n 0) true (odd (- n 1)))))\n\
    \         (odd (lambda (n) (if (= n 0) false (even (- n 1))))))\n\
    \  (even 10))";
  (* Internal defines can use later siblings and capture the enclosing scope. *)
  evaluates "12"
    "(define (f base)\n\
    \  (define (g n) (+ (h n) base))\n\
    \  (define (h n) (* n 2))\n\
    \  (g 1))\n\
     (f 10)";
  (* A closure nested inside a recursive function uses the patched capture. *)
  evaluates "(3 2 1)"
    "(define (down n)\n\
    \  (letrec ((go (lambda (k)\n\
    \                 (if (= k 0) nil\n\
    \                     ((lambda (x) (cons x (go (- k 1)))) k)))))\n\
    \    (go n)))\n\
     (down 3)";
  (* letrec captures both a sibling and an outer variable. *)
  evaluates "7"
    "(define (f x)\n\
    \  (letrec ((a (lambda (n) (if (= n 0) x (b (- n 1)))))\n\
    \           (b (lambda (n) (a n))))\n\
    \    (a 3)))\n\
     (f 7)"

let test_global_order () =
  evaluates "2" "(define (f) (g)) (define (g) 2) (f)";
  prints "calling a function before its definition" "1\n" "(print (f)) (define (f) 1)";
  prints "values run in source order" "1\n2\n"
    "(define a (begin (print 1) 1)) (define b (begin (print 2) 2))";
  fails_at_runtime "'x' was used before it was defined" "(print x) (define x 1)";
  fails_at_runtime "'x' was used before it was defined" "(define x (+ x 1))"

let test_first_class_primitives_and_constructors () =
  evaluates "6" "(foldl + 0 '(1 2 3))";
  evaluates "((some 1) (some 2))" "(map some '(1 2))";
  evaluates "(true false)" "(map (lambda (x) (= x 1)) '(1 2))";
  evaluates "\"ab\"" "(foldl string-append \"\" '(\"a\" \"b\"))";
  evaluates "3" "(let ((plus +)) (plus 1 2))"

let test_shadowing () =
  evaluates "\"x\"" "(define (+ a b) \"x\") (+ 1 2)";
  evaluates "10" "(let ((+ *)) (+ 2 5))";
  (* Replacing a standard library function doesn't change code already compiled against it. *)
  evaluates "2" "(define (length xs) 99) (let ((r (reverse '(1 2)))) (head r))";
  evaluates "99" "(define (length xs) 99) (length nil)";
  evaluates "2" "(type (pair a) (pair a a)) (match (pair 1 2) ((pair _ b) b))"

let tree =
  "(type (tree a) leaf (node (tree a) a (tree a)))\n\
   (define (insert x t)\n\
  \  (match t\n\
  \    (leaf (node leaf x leaf))\n\
  \    ((node l v r)\n\
  \     (cond ((< x v) (node (insert x l) v r))\n\
  \           ((> x v) (node l v (insert x r)))\n\
  \           (else t)))))\n\
   (define (to-list t)\n\
  \  (match t\n\
  \    (leaf nil)\n\
  \    ((node l v r) (append (to-list l) (cons v (to-list r))))))\n"

let test_data_types () =
  evaluates "(1 2 3 5 8)" (tree ^ "(to-list (foldl (flip insert) leaf '(5 1 8 3 2)))");
  evaluates "(node leaf 1 leaf)" (tree ^ "(insert 1 leaf)");
  evaluates "(pair 1 \"a\")" "(pair 1 \"a\")"

let test_pattern_matching () =
  evaluates "\"two\"" "(match 2 (1 \"one\") (2 \"two\") (_ \"many\"))";
  evaluates "\"many\"" "(match 3 (1 \"one\") (2 \"two\") (_ \"many\"))";
  evaluates "\"b\"" "(match \"x\" (\"y\" \"a\") (\"x\" \"b\") (_ \"c\"))";
  evaluates "1" "(match true (true 1) (false 2))";
  evaluates "2" "(match false (true 1) (false 2))";
  evaluates "1" "(match unit (unit 1))";
  evaluates "3" "(match 3 (n n))";
  (* A literal beyond the native int range. *)
  evaluates "\"big\""
    "(match 100000000000000000000 (100000000000000000000 \"big\") (_ \"small\"))";
  evaluates "3" "(match '(1 2) ((list a b) (+ a b)) (_ 0))";
  evaluates "0" "(match '(1 2 3) ((list a b) (+ a b)) (_ 0))";
  evaluates "2" "(match (pair 1 2) ((pair _ b) b))";
  evaluates "\"nested\""
    "(match (some (some 1)) ((some (some 1)) \"nested\") (_ \"other\"))";
  evaluates "\"other\""
    "(match (some (some 2)) ((some (some 1)) \"nested\") (_ \"other\"))";
  evaluates "\"other\"" "(match (some none) ((some (some 1)) \"nested\") (_ \"other\"))"

(* Clause order must decide, including when literal, constructor and wildcard rows mix and a
   failed nested test has to fall through to a later clause. *)
let test_match_backtracking () =
  let classify =
    "(define (f xs)\n\
    \  (match xs\n\
    \    ((cons 1 (cons 2 nil)) \"one-two\")\n\
    \    ((cons 1 _) \"one\")\n\
    \    ((cons _ nil) \"single\")\n\
    \    (nil \"empty\")\n\
    \    (_ \"other\")))\n"
  in
  let cases =
    [
      ("'(1 2)", "one-two");
      ("'(1 2 3)", "one");
      ("'(1)", "one");
      ("'(9)", "single");
      ("nil", "empty");
      ("'(9 9)", "other");
    ]
  in
  List.iter
    (fun (input, expected) ->
      evaluates ("\"" ^ expected ^ "\"") (classify ^ "(f " ^ input ^ ")"))
    cases;
  let mixed =
    "(define (g p)\n\
    \  (match p\n\
    \    ((pair 0 _) \"zero-left\")\n\
    \    ((pair _ 0) \"zero-right\")\n\
    \    ((pair 1 1) \"ones\")\n\
    \    (_ \"other\")))\n"
  in
  evaluates "\"zero-left\"" (mixed ^ "(g (pair 0 0))");
  evaluates "\"zero-right\"" (mixed ^ "(g (pair 5 0))");
  evaluates "\"ones\"" (mixed ^ "(g (pair 1 1))");
  evaluates "\"other\"" (mixed ^ "(g (pair 1 2))");
  (* A match used as an operand: failing over must leave the operand stack intact. *)
  evaluates "11" "(+ 1 (match '(1) ((cons 2 _) 5) ((cons 1 _) 10) (_ 0)))";
  (* A match nested in a clause body doesn't disturb the outer one. *)
  evaluates "\"inner\""
    "(match '(1 2)\n\
    \  ((cons 9 _) \"no\")\n\
    \  ((cons _ rest) (match rest ((cons 3 _) \"no\") ((cons _ _) \"inner\") (nil \
     \"no\")))\n\
    \  (nil \"no\"))"

let test_equality () =
  evaluates "true" "(= 1 1)";
  evaluates "false" "(= 1 2)";
  evaluates "true" "(!= 1 2)";
  evaluates "true" "(= \"a\" \"a\")";
  evaluates "true" "(= '(1 2) '(1 2))";
  evaluates "false" "(= '(1 2) '(1 3))";
  evaluates "false" "(= '(1 2) '(1))";
  evaluates "true" "(= (some 1) (some 1))";
  evaluates "false" "(= (some 1) none)";
  evaluates "true" "(= true true)";
  evaluates "true" "(= (pow 10 30) (pow 10 30))";
  evaluates "false" "(= (pow 10 30) 1)";
  fails_at_runtime "functions cannot be compared" "(= id id)";
  fails_at_runtime "functions cannot be compared" "(= (some id) (some id))"

let big = Bigint.of_string

let test_integer_promotion () =
  evaluates "265252859812191058636308480000000"
    "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 30)";
  let max_i = string_of_int max_int and min_i = string_of_int min_int in
  let sum = Bigint.add (big max_i) Bigint.one in
  evaluates (Bigint.to_string sum) ("(+ " ^ max_i ^ " 1)");
  evaluates max_i ("(- (+ " ^ max_i ^ " 1) 1)");
  evaluates (Bigint.to_string (Bigint.neg (big min_i))) ("(neg " ^ min_i ^ ")");
  evaluates (Bigint.to_string (Bigint.neg (big min_i))) ("(quot " ^ min_i ^ " -1)");
  evaluates "0" ("(rem " ^ min_i ^ " -1)");
  evaluates (Bigint.to_string (Bigint.sub (big min_i) Bigint.one)) ("(- " ^ min_i ^ " 1)");
  (* Comparison works across the two representations. *)
  evaluates "true" ("(< " ^ max_i ^ " (+ " ^ max_i ^ " 1))");
  evaluates "true" ("(> " ^ min_i ^ " (- " ^ min_i ^ " 1))");
  (* A computed value that fits returns to the small form, so = doesn't depend on history. *)
  evaluates "true" ("(= 5 (- (+ " ^ max_i ^ " 5) " ^ max_i ^ "))")

let test_integer_ops () =
  evaluates "(3 1)" "(list (quot 7 2) (rem 7 2))";
  evaluates "(-3 -1)" "(list (quot -7 2) (rem -7 2))";
  (* Floored division in the prelude. *)
  evaluates "(-4 1)" "(list (div -7 2) (mod -7 2))";
  evaluates "(-4 -1)" "(list (div 7 -2) (mod 7 -2))";
  evaluates "(3 1)" "(list (div 7 2) (mod 7 2))";
  evaluates "(3 -1)" "(list (div -7 -2) (mod -7 -2))";
  evaluates "(2 0)" "(list (div 6 3) (mod 6 3))";
  evaluates "1024" "(pow 2 10)";
  evaluates "6" "(gcd 54 24)";
  evaluates "5" "(abs -5)";
  fails_at_runtime "division by zero" "(quot 1 0)";
  fails_at_runtime "division by zero" "(rem 1 0)";
  fails_at_runtime "division by zero" "(quot (pow 10 30) 0)";
  fails_at_runtime "negative exponent" "(pow 2 -1)"

let test_strings () =
  evaluates "\"foobar\"" "(string-append \"foo\" \"bar\")";
  evaluates "3" "(string-length \"foo\")";
  evaluates "\"oo\"" "(substring \"foo\" 1 2)";
  evaluates "\"\"" "(substring \"foo\" 3 0)";
  evaluates "\"a,b,c\"" "(string-join \",\" '(\"a\" \"b\" \"c\"))";
  evaluates "\"\"" "(string-join \",\" nil)";
  fails_at_runtime "out of range" "(substring \"foo\" 1 5)";
  fails_at_runtime "out of range" "(substring \"foo\" -1 1)";
  fails_at_runtime "out of range" "(substring \"foo\" 0 -1)";
  fails_at_runtime "out of range" "(substring \"foo\" (pow 10 30) 1)"

let test_prelude () =
  evaluates "(2 3 4)" "(map (lambda (x) (+ x 1)) '(1 2 3))";
  evaluates "(2 4)" "(filter even? '(1 2 3 4))";
  evaluates "(1 2 3 4)" "(append '(1 2) '(3 4))";
  evaluates "(3 2 1)" "(reverse '(1 2 3))";
  evaluates "3" "(length '(1 2 3))";
  evaluates "6" "(sum '(1 2 3))";
  evaluates "24" "(product '(1 2 3 4))";
  evaluates "(1 2 3 4)" "(concat '((1) (2 3) () (4)))";
  evaluates "(1 1 2 2)" "(concat-map (lambda (x) (list x x)) '(1 2))";
  evaluates "(0 1 2)" "(range 0 3)";
  evaluates "()" "(range 3 3)";
  evaluates "()" "(range 5 3)";
  evaluates "(1 2)" "(take 2 '(1 2 3))";
  evaluates "(3)" "(drop 2 '(1 2 3))";
  evaluates "()" "(take 0 '(1))";
  evaluates "(1)" "(drop 0 '(1))";
  evaluates "()" "(drop 5 '(1))";
  evaluates "((pair 1 \"a\") (pair 2 \"b\"))" "(zip '(1 2 3) '(\"a\" \"b\"))";
  evaluates "2" "(nth '(1 2 3) 1)";
  evaluates "true" "(member? 2 '(1 2))";
  evaluates "false" "(member? 3 '(1 2))";
  evaluates "true" "(any? even? '(1 2))";
  evaluates "false" "(all? even? '(1 2))";
  evaluates "(some 2)" "(find even? '(1 2 3 4))";
  evaluates "none" "(find even? '(1 3))";
  evaluates "(1 1 2 3 5 9)" "(sort < '(3 1 5 1 9 2))";
  evaluates "(9 5 3)" "(sort > '(3 9 5))";
  evaluates "()" "(sort < nil)";
  (* The sort is stable: equal keys stay in their original order. *)
  evaluates "((pair 1 \"a\") (pair 1 \"b\") (pair 2 \"c\"))"
    "(sort (lambda (x y) (< (fst x) (fst y)))\n\
    \  (list (pair 2 \"c\") (pair 1 \"a\") (pair 1 \"b\")))";
  evaluates "5" "(with-default 5 none)";
  evaluates "(some 2)" "(map-option (lambda (x) (+ x 1)) (some 1))";
  evaluates "2" "((compose (lambda (x) (+ x 1)) (lambda (x) (* x 1))) 1)";
  evaluates "(pair 2 1)" "((flip pair) 1 2)";
  prints "for-each" "1\n2\n" "(for-each print '(1 2))";
  fails_at_runtime "head: empty list" "(head nil)";
  fails_at_runtime "tail: empty list" "(tail nil)";
  fails_at_runtime "nth: index out of range" "(nth '(1) 1)"

let test_tail_calls_use_constant_stack () =
  (* A frame takes at least 3 slots, so the 16-million-slot stack limit would stop these well
     short of 10 million calls if frames accumulated. *)
  evaluates "0" "(define (loop n) (if (= n 0) 0 (loop (- n 1)))) (loop 10000000)";
  (* Mutual tail calls and tail calls through match and let. *)
  evaluates "true"
    "(define (even n) (if (= n 0) true (odd (- n 1))))\n\
     (define (odd n) (if (= n 0) false (even (- n 1))))\n\
     (even 10000000)";
  evaluates "0"
    "(define (loop n)\n\
    \  (match n\n\
    \    (0 0)\n\
    \    (_ (let ((m (- n 1))) (begin unit (loop m))))))\n\
     (loop 10000000)";
  (* A tail call to a function with a different number of parameters and a different frame
     size. *)
  evaluates "7"
    "(define (a n) (if (= n 0) 7 (b n 1 2 3 4 5)))\n\
     (define (b n w x y z q) (let ((m (- n 1))) (a m)))\n\
     (a 5000000)"

let test_deep_recursion () =
  (* Far deeper than the native stack could go, because calls don't use it. *)
  evaluates "1000000"
    "(define (count n) (if (= n 0) 0 (+ 1 (count (- n 1))))) (count 1000000)";
  evaluates "1000000" "(length (map id (range 0 1000000)))";
  (* Runaway recursion is an ordinary error. *)
  fails_at_runtime "stack overflow" "(define (forever n) (+ 1 (forever n))) (forever 1)"

(* Long lists must not overflow the native stack in printing or equality. *)
let test_long_lists () =
  let n = 1_000_000 in
  let xs = Printf.sprintf "(define xs (range 0 %d))\n" n in
  evaluates "true" (xs ^ "(= xs (map id xs))");
  evaluates "false" (xs ^ "(= xs (cons 1 xs))");
  (* "(0 1 ... n-1)": every number's digits, a space between each, and two parentheses. *)
  let digits = ref 0 in
  for i = 0 to n - 1 do
    digits := !digits + String.length (string_of_int i)
  done;
  evaluates (string_of_int (!digits + (n - 1) + 2)) (xs ^ "(string-length (show xs))")

let test_stack_trace () =
  let program =
    "(define (a) (+ 1 (b)))\n(define (b) (+ 1 (c)))\n(define (c) (quot 1 0))\n(a)"
  in
  match run program with
  | _ -> Alcotest.fail "expected a runtime error"
  | exception Vm.Runtime_error ({ message; trace; _ } as error) ->
      Alcotest.(check string) "message" "division by zero" message;
      Alcotest.(check (list string))
        "calling functions" [ "c"; "b"; "a"; "<main>" ]
        (List.map (fun (f : Vm.frame) -> f.fn_name) trace);
      Alcotest.(check (list string))
        "locations"
        [ "test.sl:3:13"; "test.sl:2:18"; "test.sl:1:18"; "test.sl:4:1" ]
        (List.map (fun (f : Vm.frame) -> Diagnostic.location f.location) trace);
      Alcotest.(check string)
        "rendered"
        "runtime error: division by zero\n\
        \  in c at test.sl:3:13\n\
        \  in b at test.sl:2:18\n\
        \  in a at test.sl:1:18\n\
        \  in <main> at test.sl:4:1\n"
        (Vm.render_error error)

let test_long_trace_is_abbreviated () =
  match
    run "(define (deep n) (if (= n 0) (quot 1 0) (+ 1 (deep (- n 1))))) (deep 1000)"
  with
  | _ -> Alcotest.fail "expected a runtime error"
  | exception Vm.Runtime_error ({ trace; hidden_calls; _ } as error) ->
      (* 1001 calls to deep, plus <main>, with 5 kept from each end. *)
      Alcotest.(check int) "frames kept" 10 (List.length trace);
      Alcotest.(check int) "frames hidden" 992 hidden_calls;
      Alcotest.(check string) "innermost" "deep" (List.hd trace).fn_name;
      Alcotest.(check string) "outermost" "<main>" (List.hd (List.rev trace)).fn_name;
      Alcotest.(check bool)
        "abbreviated" true
        (contains ~fragment:"... 992 more calls ..." (Vm.render_error error))

(* REPL-style sessions: each eval sees what came before. *)

let session_eval session text = Driver.eval session { name = "<repl>"; text }

let test_session_persistence () =
  let session = Driver.create ~out:ignore () in
  let value text = Value.repr (session_eval session text).value in
  ignore (value "(define x 10)");
  ignore (value "(define (add-x y) (+ x y))");
  Alcotest.(check string) "uses earlier definitions" "15" (value "(add-x 5)");
  (* A redefinition shadows, but compiled code keeps the binding it saw. *)
  ignore (value "(define x \"now a string\")");
  Alcotest.(check string) "new binding" "\"now a string\"" (value "x");
  Alcotest.(check string) "earlier code unaffected" "15" (value "(add-x 5)");
  ignore (value "(type box (box int))");
  Alcotest.(check string) "types persist" "(box 1)" (value "(box 1)");
  (* Redefining a type makes a genuinely new type. *)
  ignore (value "(define old-box (box 1))");
  ignore (value "(type box (box string))");
  Alcotest.(check string) "new constructor" "(box \"s\")" (value "(box \"s\")");
  (match value "(= old-box (box \"s\"))" with
  | _ -> Alcotest.fail "old and new box are different types"
  | exception Diagnostic.Error _ -> ());
  (* Many definitions force the globals array to grow past its initial size. *)
  for i = 0 to 600 do
    ignore (value (Printf.sprintf "(define g%d %d)" i i))
  done;
  Alcotest.(check string) "many globals" "600" (value "g600")

let test_session_survives_errors () =
  let session = Driver.create ~out:ignore () in
  let value text = Value.repr (session_eval session text).value in
  ignore (value "(define x 1)");
  (* A type error changes nothing. *)
  (match value "(define x \"s\") (+ x 1)" with
  | _ -> Alcotest.fail "expected a type error"
  | exception Diagnostic.Error _ -> ());
  Alcotest.(check string) "after a type error" "2" (value "(+ x 1)");
  (* A runtime error partway through a deep stack leaves the VM usable. *)
  (match
     value "(define (deep n) (if (= n 0) (quot 1 0) (+ 1 (deep (- n 1))))) (deep 100000)"
   with
  | _ -> Alcotest.fail "expected a runtime error"
  | exception Vm.Runtime_error _ -> ());
  Alcotest.(check string) "after a runtime error" "3" (value "(+ x 2)");
  (* Even a stack overflow. *)
  (match value "(define (f n) (+ 1 (f n))) (f 1)" with
  | _ -> Alcotest.fail "expected a stack overflow"
  | exception Vm.Runtime_error _ -> ());
  Alcotest.(check string) "after a stack overflow" "4" (value "(+ x 3)")

let test_driver_helpers () =
  let session = Driver.create ~out:ignore () in
  let source text : Diagnostic.source = { name = "t"; text } in
  Alcotest.(check (list (pair string string)))
    "check"
    [ ("f", "(-> int int)"); ("g", "(-> a a)") ]
    (List.map
       (fun (name, scheme) -> (name, Types.scheme_to_string scheme))
       (Driver.check session (source "(define (f x) (+ x 1)) (define (g y) y)")));
  (* check doesn't change the session. *)
  (match Driver.eval session (source "f") with
  | _ -> Alcotest.fail "check should not define anything"
  | exception Diagnostic.Error _ -> ());
  Alcotest.(check string)
    "type_of" "(-> (list a) int)"
    (Types.scheme_to_string (Driver.type_of session (source "length")));
  match Driver.type_of session (source "1 2") with
  | _ -> Alcotest.fail "type_of takes one expression"
  | exception Diagnostic.Error _ -> ()

let test_outcome_types () =
  let session = Driver.create ~out:ignore () in
  let outcome = session_eval session "(define (f x) x) (f 1)" in
  Alcotest.(check (option string))
    "value type" (Some "int")
    (Option.map Types.scheme_to_string outcome.value_type);
  Alcotest.(check (list string)) "defined" [ "f" ] (List.map fst outcome.definitions)

(* Integer arithmetic must agree with Zarith on every operand, especially near the points where
   a fast path stops being valid. These run real programs, so they also cover the fast paths
   inlined in the VM's dispatch loop. *)

let gen_integer =
  let open QCheck2.Gen in
  let near boundary =
    let* offset = int_range (-3) 3 in
    let* negative = bool in
    let z = Z.add boundary (Z.of_int offset) in
    pure (if negative then Z.neg z else z)
  in
  let random_big =
    let* digits = int_range 1 60 in
    let* s = string_size ~gen:(char_range '0' '9') (pure digits) in
    pure (Z.of_string s)
  in
  oneof_weighted
    [
      (3, map Z.of_int (int_range (-1000) 1000));
      (2, map Z.of_int int);
      (* Where multiplication's fast path ends. *)
      (3, near (Z.shift_left Z.one 31));
      (* Their product is the first to overflow a native int. *)
      (3, near (Z.shift_left Z.one 40));
      (* The limits of a native int. *)
      (4, near (Z.of_int max_int));
      (4, near (Z.of_int min_int));
      (2, near (Z.shift_left Z.one 62));
      (2, random_big);
    ]

(* One session for all cases, since each one only compiles a line of code. *)
let arithmetic_session = lazy (Driver.create ~out:ignore ())

let eval_integer text =
  let outcome = Driver.eval (Lazy.force arithmetic_session) { name = "arith"; text } in
  outcome.value

let binary name ~skip_zero_divisor op_z =
  QCheck2.Test.make
    ~name:(name ^ " agrees with Zarith")
    ~count:300
    ~print:(fun (a, b) -> Z.to_string a ^ ", " ^ Z.to_string b)
    QCheck2.Gen.(pair gen_integer gen_integer)
    (fun (a, b) ->
      QCheck2.assume (not (skip_zero_divisor && Z.equal b Z.zero));
      let text = Printf.sprintf "(%s %s %s)" name (Z.to_string a) (Z.to_string b) in
      let expected = op_z a b in
      let value = eval_integer text in
      let matches = Value.repr value = Z.to_string expected in
      (* One number must have one representation. *)
      let canonical =
        match value with
        | Bytecode.Int _ -> Z.fits_int expected
        | Bytecode.Big _ -> not (Z.fits_int expected)
        | _ -> false
      in
      matches && canonical)

let comparison name op_z =
  QCheck2.Test.make
    ~name:(name ^ " agrees with Zarith")
    ~count:300
    ~print:(fun (a, b) -> Z.to_string a ^ ", " ^ Z.to_string b)
    QCheck2.Gen.(pair gen_integer gen_integer)
    (fun (a, b) ->
      let text = Printf.sprintf "(%s %s %s)" name (Z.to_string a) (Z.to_string b) in
      Value.repr (eval_integer text) = string_of_bool (op_z a b))

(* The combined instructions take their left operand from a local and their right from a small
   constant, so they need operands in that shape, compiled inside a function. *)
let gen_small_constant =
  QCheck2.Gen.(
    oneof_weighted
      [
        (5, int_range (-3) 3);
        (2, pure max_int);
        (2, pure min_int);
        (1, pure (max_int - 1));
        (1, pure (min_int + 1));
        (2, int);
      ])

let local_op name op_z =
  QCheck2.Test.make
    ~name:(name ^ " on a local and a constant agrees with Zarith")
    ~count:300
    ~print:(fun (a, k) -> Z.to_string a ^ ", " ^ string_of_int k)
    QCheck2.Gen.(pair gen_integer gen_small_constant)
    (fun (a, k) ->
      let text = Printf.sprintf "(let ((x %s)) (%s x %d))" (Z.to_string a) name k in
      let expected = op_z a (Z.of_int k) in
      let value = eval_integer text in
      Value.repr value = Z.to_string expected
      &&
      match value with
      | Bytecode.Int _ -> Z.fits_int expected
      | Bytecode.Big _ -> not (Z.fits_int expected)
      | _ -> false)

let local_branch name op_z =
  QCheck2.Test.make
    ~name:(name ^ " branching on a local and a constant agrees with Zarith")
    ~count:300
    ~print:(fun (a, k) -> Z.to_string a ^ ", " ^ string_of_int k)
    QCheck2.Gen.(pair gen_integer gen_small_constant)
    (fun (a, k) ->
      let text =
        Printf.sprintf "(let ((x %s)) (if (%s x %d) true false))" (Z.to_string a) name k
      in
      Value.repr (eval_integer text) = string_of_bool (op_z a (Z.of_int k)))

let arithmetic_properties =
  [
    local_op "+" Z.add;
    local_op "-" Z.sub;
    local_branch "<" Z.lt;
    local_branch "<=" Z.leq;
    local_branch ">" Z.gt;
    local_branch ">=" Z.geq;
    local_branch "=" Z.equal;
    local_branch "!=" (fun a b -> not (Z.equal a b));
    binary "+" ~skip_zero_divisor:false Z.add;
    binary "-" ~skip_zero_divisor:false Z.sub;
    binary "*" ~skip_zero_divisor:false Z.mul;
    binary "quot" ~skip_zero_divisor:true Z.div;
    binary "rem" ~skip_zero_divisor:true Z.rem;
    comparison "<" Z.lt;
    comparison "<=" Z.leq;
    comparison ">" Z.gt;
    comparison ">=" Z.geq;
    comparison "=" Z.equal;
    comparison "!=" (fun a b -> not (Z.equal a b));
    QCheck2.Test.make ~name:"neg agrees with Zarith" ~count:300 ~print:Z.to_string
      gen_integer (fun a ->
        Value.repr (eval_integer (Printf.sprintf "(neg %s)" (Z.to_string a)))
        = Z.to_string (Z.neg a));
  ]

(* Code generation *)

let disassemble text =
  Driver.disassemble (Driver.create ~out:ignore ()) { name = "t"; text }

let has_instr instr text = contains ~fragment:instr (disassemble text)

let test_tail_position () =
  let tail_call = "(define (f n) (f n))" in
  Alcotest.(check bool) "self call" true (has_instr "TailCall 1" tail_call);
  Alcotest.(check bool) "not a plain call" false (has_instr "  Call 1" tail_call);
  let in_branches = "(define (f n) (if (= n 0) (f 1) (f 2)))" in
  Alcotest.(check bool) "in if" false (has_instr "  Call 1" in_branches);
  let in_match = "(define (f n) (match n (0 (f 1)) (_ (f 2))))" in
  Alcotest.(check bool) "in match" false (has_instr "  Call 1" in_match);
  let in_let = "(define (f n) (let ((m n)) (f m)))" in
  Alcotest.(check bool) "in let" false (has_instr "  Call 1" in_let);
  let not_tail = "(define (f n) (+ 1 (f n)))" in
  Alcotest.(check bool) "operand is not a tail call" true (has_instr "  Call 1" not_tail);
  let in_seq = "(define (f n) (begin (f 1) 2))" in
  Alcotest.(check bool) "not last in sequence" true (has_instr "  Call 1" in_seq)

let test_disassembly () =
  let listing = disassemble "(define (add1 n) (+ n 1)) (print (add1 41))" in
  List.iter
    (fun fragment -> Alcotest.(check bool) fragment true (contains ~fragment listing))
    [
      "add1 (arity 1, locals 1, frame 2)";
      "<main> (arity 0";
      "AddLocalInt local 0, 1";
      "Push 41";
      "Return";
      "MakeClosure add1";
      "Print";
    ]

(* The stack depth checker has to reject malformed code, or passing it means nothing. *)
(* Locals whose scopes don't overlap share a slot, which keeps frames small. *)
let test_slots_are_reused () =
  let locals text n =
    let fragment = Printf.sprintf "f (arity 0, locals %d," n in
    Alcotest.(check bool) text true (contains ~fragment (disassemble text))
  in
  locals "(define (f) (begin (let ((a 1)) a) (let ((b 2)) b)))" 1;
  locals "(define (f) (let ((a 1)) (let ((b 2)) (+ a b))))" 2

(* The combined forms must actually get used, or their speed benefit silently disappears. *)
let test_combined_instructions_are_used () =
  let uses fragment text =
    Alcotest.(check bool) (fragment ^ " in " ^ text) true (has_instr fragment text)
  in
  let avoids fragment text =
    Alcotest.(check bool) (fragment ^ " not in " ^ text) false (has_instr fragment text)
  in
  uses "AddLocalInt local 0, 1" "(define (f n) (+ n 1))";
  uses "AddLocalInt local 0, 1" "(define (f n) (+ 1 n))";
  uses "SubLocalInt local 0, 1" "(define (f n) (- n 1))";
  (* Subtraction isn't commutative, so a constant on the left must not use it. *)
  avoids "SubLocalInt" "(define (f n) (- 1 n))";
  uses "JumpUnlessLocalInt local 0 < 2" "(define (f n) (if (< n 2) 1 2))";
  uses "JumpUnlessLocalInt local 0 = 0" "(define (f n) (match n (0 1) (_ 2)))";
  avoids "JumpUnlessLocalInt" "(define (f s) (match s (\"a\" 1) (_ 2)))";
  (* A captured variable isn't in a local slot. *)
  avoids "AddLocalInt" "(define (f n) (lambda () (+ n 1)))";
  (* A shadowed operator isn't the primitive. *)
  avoids "AddLocalInt" "(define (f + n) (+ n 1))"

let test_stack_checker () =
  let open Bytecode in
  let rejects name code =
    match max_operand_depth code with
    | _ -> Alcotest.failf "%s should have been rejected" name
    | exception Failure _ -> ()
  in
  rejects "underflow" [| Pop; Return |];
  rejects "underflow in call" [| Call 1 |];
  rejects "bad jump" [| Jump 5 |];
  rejects "unpatched jump" [| Jump (-1) |];
  rejects "falls off the end" [| Push Unit |];
  rejects "branches disagree on depth"
    [| Push True; JumpIfFalse 3; Push Unit; Push Unit; Return |];
  Alcotest.(check int)
    "deepest" 2
    (max_operand_depth [| Push Unit; Push Unit; Add; Return |]);
  Alcotest.(check int)
    "unreachable code is ignored" 1
    (max_operand_depth [| Push Unit; Return; Pop; Pop; Pop |])

let suite =
  [
    Alcotest.test_case "basics" `Quick test_basics;
    Alcotest.test_case "short circuit" `Quick test_short_circuit;
    Alcotest.test_case "printing values" `Quick test_printing_values;
    Alcotest.test_case "closures" `Quick test_closures;
    Alcotest.test_case "local recursion" `Quick test_local_recursion;
    Alcotest.test_case "global order" `Quick test_global_order;
    Alcotest.test_case "first-class primitives and constructors" `Quick
      test_first_class_primitives_and_constructors;
    Alcotest.test_case "shadowing" `Quick test_shadowing;
    Alcotest.test_case "data types" `Quick test_data_types;
    Alcotest.test_case "pattern matching" `Quick test_pattern_matching;
    Alcotest.test_case "match backtracking" `Quick test_match_backtracking;
    Alcotest.test_case "equality" `Quick test_equality;
    Alcotest.test_case "integer promotion" `Quick test_integer_promotion;
    Alcotest.test_case "integer ops" `Quick test_integer_ops;
    Alcotest.test_case "strings" `Quick test_strings;
    Alcotest.test_case "prelude" `Quick test_prelude;
    Alcotest.test_case "tail calls use constant stack" `Quick
      test_tail_calls_use_constant_stack;
    Alcotest.test_case "deep recursion" `Quick test_deep_recursion;
    Alcotest.test_case "long lists" `Quick test_long_lists;
    Alcotest.test_case "stack trace" `Quick test_stack_trace;
    Alcotest.test_case "long trace is abbreviated" `Quick test_long_trace_is_abbreviated;
    Alcotest.test_case "session persistence" `Quick test_session_persistence;
    Alcotest.test_case "session survives errors" `Quick test_session_survives_errors;
    Alcotest.test_case "driver helpers" `Quick test_driver_helpers;
    Alcotest.test_case "outcome types" `Quick test_outcome_types;
    Alcotest.test_case "tail position" `Quick test_tail_position;
    Alcotest.test_case "disassembly" `Quick test_disassembly;
    Alcotest.test_case "slots are reused" `Quick test_slots_are_reused;
    Alcotest.test_case "combined instructions are used" `Quick
      test_combined_instructions_are_used;
    Alcotest.test_case "stack checker" `Quick test_stack_checker;
  ]
  @ List.map QCheck_alcotest.to_alcotest arithmetic_properties

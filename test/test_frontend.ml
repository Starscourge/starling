open Starling

let source text : Diagnostic.source = { name = "test.sl"; text }
let read text = List.map Reader.to_string (Reader.read (source text))
let strings = Alcotest.(list string)

(* Asserts that [f] reports an error containing [fragment]. *)
let reports fragment f =
  match f () with
  | _ -> Alcotest.failf "expected an error containing %S" fragment
  | exception Diagnostic.Error { message; _ } ->
      let contains =
        let n = String.length fragment in
        let rec go i =
          i + n <= String.length message
          && (String.sub message i n = fragment || go (i + 1))
        in
        go 0
      in
      if not contains then
        Alcotest.failf "expected an error containing %S, got %S" fragment message

let test_atoms () =
  Alcotest.check strings "atoms"
    [ "42"; "-7"; "foo"; "-"; "->"; "a-b"; "<="; "\"hi\"" ]
    (read "42 -7 foo - -> a-b <= \"hi\"")

let test_big_literal () =
  Alcotest.check strings "beyond native int"
    [ "123456789012345678901234567890" ]
    (read "123456789012345678901234567890")

let test_nesting_and_comments () =
  Alcotest.check strings "nesting" [ "(a (b c) ())"; "d" ]
    (read "; leading comment\n(a (b c) ()) ; trailing\n;; another\nd")

let test_quote () =
  Alcotest.check strings "quote" [ "(quote x)"; "(quote (1 2))" ] (read "'x '(1 2)")

let test_string_escapes () =
  match Reader.read (source {|"a\nb\t\"c\"\\"|}) with
  | [ { node = Str s; _ } ] -> Alcotest.(check string) "decoded" "a\nb\t\"c\"\\" s
  | _ -> Alcotest.fail "expected one string"

let test_lexical_errors () =
  reports "unknown escape" (fun () -> read {|"\q"|});
  reports "malformed number" (fun () -> read "12a");
  reports "unexpected character" (fun () -> read "[1]");
  reports "unexpected ')'" (fun () -> read ")");
  reports "unclosed '('" (fun () -> read "(a (b)");
  reports "unterminated string" (fun () -> read "\"abc")

let test_is_complete () =
  let check text expected =
    Alcotest.(check bool) text expected (Reader.is_complete text)
  in
  check "(+ 1 2)" true;
  check "" true;
  check "(+ 1" false;
  check "(a (b)" false;
  check "\"abc" false;
  check "'" false;
  check "; just a comment" true;
  (* Stray closers and bad tokens are errors now, not reasons to wait for more input. *)
  check ")" true;
  check "12a" true

let test_spans () =
  match Reader.read (source "\n  (foo bar)") with
  | [ { span; node = List [ _; second ] } ] ->
      Alcotest.(check string) "list" "test.sl:2:3" (Diagnostic.location span);
      Alcotest.(check string) "atom" "test.sl:2:8" (Diagnostic.location second.span)
  | _ -> Alcotest.fail "expected one list"

let test_rendering () =
  let source = source "(define x 1)\n(+ x  \"two\")\n" in
  let diagnostic : Diagnostic.t =
    {
      message = "type mismatch";
      span = { source; start = 19; stop = 24 };
      notes = [ "expected int" ];
    }
  in
  Alcotest.(check string)
    "report"
    "error: type mismatch\n\
    \ --> test.sl:2:7\n\
    \  |\n\
     2 | (+ x  \"two\")\n\
    \  |       ^^^^^\n\
     note: expected int\n"
    (Diagnostic.render diagnostic)

(* Parsing *)

let parse_expr text =
  match Reader.read (source text) with
  | [ form ] -> Parse.expr form
  | _ -> Alcotest.fail "expected one form"

let parse text = Parse.program (Reader.read (source text))

let test_valid_forms () =
  let accepted text = ignore (parse_expr text) in
  List.iter accepted
    [
      "(if true 1 2)";
      "(cond ((< 1 2) 3) (else 4))";
      "(and)";
      "(or a b c)";
      "(let ((x 1) (y 2)) x y)";
      "(let* ((x 1) (y x)) y)";
      "(letrec ((f (lambda (n) (f n)))) (f 1))";
      "(lambda (x _ _) x)";
      "(lambda () 1)";
      "(begin 1 2)";
      "(match x ((node l v r) v) ((leaf) 0) (_ 1))";
      "(match x ((list a b) a) (() 0) (-1 2) (\"s\" 3))";
      "'(1 (2 3) \"x\" true)";
      "(the (-> int (list a)) f)";
      "(list 1 2)";
      "(+ 1 2 3 4)";
      "((lambda (x) x) 1)";
      "()";
    ]

let test_sugar_is_expanded () =
  (match (parse_expr "(+ 1 2 3)").desc with
  | App ({ desc = Var "+"; _ }, [ { desc = App _; _ }; { desc = Int _; _ } ]) -> ()
  | _ -> Alcotest.fail "+ should fold from the left");
  (match (parse_expr "(cond (a 1) (else 2))").desc with
  | If _ -> ()
  | _ -> Alcotest.fail "cond should become if");
  (match (parse_expr "(and a b)").desc with
  | If (_, _, { desc = Bool false; _ }) -> ()
  | _ -> Alcotest.fail "and should short-circuit through if");
  match (parse_expr "(let* ((a 1) (b 2)) b)").desc with
  | Let ([ _ ], { desc = Let ([ _ ], _); _ }) -> ()
  | _ -> Alcotest.fail "let* should nest"

let test_internal_defines () =
  match
    parse "(define (f x) (define (g y) (h y)) (define (h y) y) (define z 1) (g z))"
  with
  | [ Define (_, { desc = Lambda (_, body); _ }) ] -> (
      match body.desc with
      | LetRec ([ _; _ ], { desc = Let ([ _ ], _); _ }) -> ()
      | _ -> Alcotest.fail "functions should group into one letrec, then a let")
  | _ -> Alcotest.fail "expected one define"

let test_top_level () =
  match
    parse
      "(type (tree a) (leaf) (node (tree a) a (tree a)))\n\
       (type color red green)\n\
       (sig id (-> a a))\n\
       (define (id x) x)\n\
       (define y 1)\n\
       (print y)"
  with
  | [ Type tree; Type color; Sig _; Define _; Define _; Expr _ ] ->
      Alcotest.(check int) "params" 1 (List.length tree.params);
      Alcotest.(check int) "ctors" 2 (List.length tree.ctors);
      Alcotest.(check int) "bare ctors" 2 (List.length color.ctors)
  | _ -> Alcotest.fail "unexpected top-level shape"

let test_form_errors () =
  let bad fragment text = reports fragment (fun () -> parse text) in
  bad "two branches" "(if a b)";
  bad "final 'else'" "(cond (a 1))";
  bad "final 'else'" "(cond)";
  bad "must come last" "(cond (else 1) (a 2))";
  bad "reserved word" "(define if 1)";
  bad "cannot be defined" "(define _ 1)";
  bad "cannot be defined" "(define (_ x) x)";
  bad "reserved word" "(lambda (let) 1)";
  bad "bound more than once" "(lambda (x x) x)";
  bad "bound more than once" "(let ((x 1) (x 2)) x)";
  bad "must be a lambda" "(letrec ((x 1)) x)";
  bad "must end with an expression" "(define (f) (define x 1))";
  bad "at least one clause" "(match x)";
  bad "not allowed here" "(lambda () (type t a))";
  bad "only allowed as the last clause" "(else 1)";
  bad "cannot be used as a value" "if";
  bad "cannot be used as a value" "_";
  bad "quoted data" "'(a b)";
  bad "takes no arguments" "(true 1)";
  bad "at least one constructor" "(type empty)";
  bad "looks like a type variable" "(type a (x))";
  bad "not a valid type variable" "(type (box foo) (x))";
  bad "expected a type" "(sig f (1 2))"

let test_type_var_shape () =
  let check name expected =
    Alcotest.(check bool) name expected (Parse.is_type_var name)
  in
  check "a" true;
  check "t1" true;
  check "z99" true;
  check "int" false;
  check "ab" false;
  check "A" false;
  check "" false

let suite =
  [
    Alcotest.test_case "atoms" `Quick test_atoms;
    Alcotest.test_case "big literal" `Quick test_big_literal;
    Alcotest.test_case "nesting and comments" `Quick test_nesting_and_comments;
    Alcotest.test_case "quote" `Quick test_quote;
    Alcotest.test_case "string escapes" `Quick test_string_escapes;
    Alcotest.test_case "lexical errors" `Quick test_lexical_errors;
    Alcotest.test_case "is_complete" `Quick test_is_complete;
    Alcotest.test_case "spans" `Quick test_spans;
    Alcotest.test_case "rendering" `Quick test_rendering;
    Alcotest.test_case "valid forms" `Quick test_valid_forms;
    Alcotest.test_case "sugar is expanded" `Quick test_sugar_is_expanded;
    Alcotest.test_case "internal defines" `Quick test_internal_defines;
    Alcotest.test_case "top level" `Quick test_top_level;
    Alcotest.test_case "form errors" `Quick test_form_errors;
    Alcotest.test_case "type variable shape" `Quick test_type_var_shape;
  ]

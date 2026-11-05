open Syntax

let reserved =
  [
    "if";
    "cond";
    "else";
    "let";
    "let*";
    "letrec";
    "lambda";
    "match";
    "begin";
    "and";
    "or";
    "quote";
    "the";
    "define";
    "type";
    "sig";
    "true";
    "false";
    "unit";
  ]

let is_reserved name = List.mem name reserved

let is_type_var name =
  let is_digit c = c >= '0' && c <= '9' in
  let rec digits i = i >= String.length name || (is_digit name.[i] && digits (i + 1)) in
  String.length name > 0 && name.[0] >= 'a' && name.[0] <= 'z' && digits 1

let binder (form : Reader.sexp) =
  match form.node with
  | Sym name when is_reserved name ->
      Diagnostic.errorf form.span "'%s' is a reserved word and cannot be used as a name"
        name
  | Sym name -> { name; binder_span = form.span }
  | _ -> Diagnostic.error form.span "expected a name"

(* '_' may repeat in a parameter list because it never binds anything. *)
let check_distinct binders =
  let rec go seen = function
    | [] -> ()
    | { name = "_"; _ } :: rest -> go seen rest
    | { name; binder_span } :: _ when List.mem name seen ->
        Diagnostic.errorf binder_span "'%s' is bound more than once here" name
    | { name; _ } :: rest -> go (name :: seen) rest
  in
  go [] binders

let mk span desc = { desc; span }

(* Types *)

let rec ty_expr (form : Reader.sexp) =
  match form.node with
  | Sym name -> { ty = TyName name; ty_span = form.span }
  | List ({ node = Sym "->"; _ } :: parts) when parts <> [] ->
      let rec split acc = function
        | [ last ] -> (List.rev acc, last)
        | x :: rest -> split (x :: acc) rest
        | [] -> assert false
      in
      let args, result = split [] parts in
      { ty = TyFun (List.map ty_expr args, ty_expr result); ty_span = form.span }
  | List ({ node = Sym name; _ } :: args) when args <> [] ->
      { ty = TyApp (name, List.map ty_expr args); ty_span = form.span }
  | _ ->
      Diagnostic.error form.span "expected a type, such as int, (list a) or (-> int bool)"

(* Patterns *)

let rec pattern (form : Reader.sexp) =
  let mk_pat pat = { pat; pat_span = form.span } in
  match form.node with
  | Sym "_" -> mk_pat PWild
  | Sym (("true" | "false" | "unit") as name) -> mk_pat (PCtor (name, []))
  | Sym name when is_reserved name ->
      Diagnostic.errorf form.span
        "'%s' is a reserved word and cannot be used in a pattern" name
  | Sym name -> mk_pat (PIdent name)
  | Int n -> mk_pat (PInt n)
  | Str s -> mk_pat (PStr s)
  | List [] -> mk_pat (PCtor ("nil", []))
  | List ({ node = Sym "list"; _ } :: items) ->
      (* (list a b) is shorthand for (cons a (cons b nil)). *)
      let cons item rest =
        { pat = PCtor ("cons", [ pattern item; rest ]); pat_span = item.span }
      in
      List.fold_right cons items (mk_pat (PCtor ("nil", [])))
  | List ({ node = Sym name; _ } :: args) -> mk_pat (PCtor (name, List.map pattern args))
  | List _ ->
      Diagnostic.error form.span
        "expected a constructor name at the start of this pattern"

(* Expressions *)

let rec quoted (form : Reader.sexp) =
  match form.node with
  | Int n -> mk form.span (Int n)
  | Str s -> mk form.span (Str s)
  | Sym "true" -> mk form.span (Bool true)
  | Sym "false" -> mk form.span (Bool false)
  | List items -> mk form.span (ListLit (List.map quoted items))
  | Sym _ ->
      Diagnostic.error form.span
        "quoted data can only contain numbers, strings, booleans and lists"

let rec expr (form : Reader.sexp) =
  match form.node with
  | Int n -> mk form.span (Int n)
  | Str s -> mk form.span (Str s)
  | Sym "true" -> mk form.span (Bool true)
  | Sym "false" -> mk form.span (Bool false)
  | Sym "unit" -> mk form.span Unit
  | Sym name when is_reserved name || name = "_" ->
      Diagnostic.errorf form.span "'%s' cannot be used as a value" name
  | Sym name -> mk form.span (Var name)
  | List [] -> mk form.span (ListLit [])
  | List ({ node = Sym head; span = head_span } :: args) ->
      special form head head_span args
  | List (head :: args) -> mk form.span (App (expr head, List.map expr args))

and special form head head_span args =
  let here = form.Reader.span in
  match (head, args) with
  | "if", [ c; t; e ] -> mk here (If (expr c, expr t, expr e))
  | "if", _ ->
      Diagnostic.errorf here
        "'if' takes a condition and two branches, but got %d arguments" (List.length args)
  | "cond", clauses -> cond here clauses
  | "and", _ ->
      fold_logic here args ~empty:true (fun a b -> If (a, b, mk here (Bool false)))
  | "or", _ ->
      fold_logic here args ~empty:false (fun a b -> If (a, mk here (Bool true), b))
  | "let", bindings :: (_ :: _ as body) ->
      let bindings = parse_bindings bindings in
      check_distinct (List.map fst bindings);
      mk here (Let (bindings, parse_body here body))
  | "let*", bindings :: (_ :: _ as body) ->
      let wrap binding inner = mk here (Let ([ binding ], inner)) in
      List.fold_right wrap (parse_bindings bindings) (parse_body here body)
  | "letrec", bindings :: (_ :: _ as body) ->
      let bindings = parse_bindings bindings in
      check_distinct (List.map fst bindings);
      List.iter
        (fun (b, rhs) ->
          match rhs.desc with
          | Lambda _ -> ()
          | _ ->
              Diagnostic.errorf b.binder_span
                "'%s' must be a lambda because 'letrec' only binds functions" b.name)
        bindings;
      mk here (LetRec (bindings, parse_body here body))
  | ("let" | "let*" | "letrec"), _ ->
      Diagnostic.errorf here "'%s' takes a list of bindings and a body" head
  | "lambda", { node = List params; _ } :: (_ :: _ as body) ->
      let params = List.map binder params in
      check_distinct params;
      mk here (Lambda (params, parse_body here body))
  | "lambda", _ -> Diagnostic.error here "'lambda' takes a parameter list and a body"
  | "begin", _ :: _ -> parse_body here args
  | "begin", [] -> Diagnostic.error here "'begin' needs at least one expression"
  | "match", scrutinee :: clauses when clauses <> [] ->
      mk here (Match (expr scrutinee, List.map parse_clause clauses))
  | "match", _ -> Diagnostic.error here "'match' takes a value and at least one clause"
  | "quote", [ data ] -> quoted data
  | "quote", _ -> Diagnostic.error here "'quote' takes exactly one form"
  | "the", [ ty; e ] -> mk here (The (ty_expr ty, expr e))
  | "the", _ -> Diagnostic.error here "'the' takes a type and an expression"
  | "list", _ -> mk here (ListLit (List.map expr args))
  | ("+" | "*"), first :: second :: (_ :: _ as rest) ->
      (* These two operators accept any number of arguments and fold from the left. *)
      let op = mk head_span (Var head) in
      let apply acc arg = mk here (App (op, [ acc; expr arg ])) in
      List.fold_left apply (mk here (App (op, [ expr first; expr second ]))) rest
  | ("define" | "type" | "sig"), _ ->
      Diagnostic.errorf here "'%s' is not allowed here" head
  | "else", _ ->
      Diagnostic.error here "'else' is only allowed as the last clause of a 'cond'"
  | ("true" | "false" | "unit"), [] -> expr { form with Reader.node = Sym head }
  | ("true" | "false" | "unit"), _ ->
      Diagnostic.errorf here "'%s' takes no arguments" head
  | _ -> mk here (App (mk head_span (Var head), List.map expr args))

and parse_bindings (form : Reader.sexp) =
  match form.node with
  | List bindings ->
      List.map
        (fun (b : Reader.sexp) ->
          match b.node with
          | List [ name; value ] -> (binder name, expr value)
          | _ -> Diagnostic.error b.span "expected a binding of the form (name value)")
        bindings
  | _ -> Diagnostic.error form.span "expected a list of bindings"

and parse_clause (clause : Reader.sexp) =
  match clause.node with
  | List (pat :: (_ :: _ as body)) -> (pattern pat, parse_body clause.span body)
  | _ -> Diagnostic.error clause.span "expected a clause of the form (pattern body)"

and fold_logic span args ~empty combine =
  match List.rev args with
  | [] -> mk span (Bool empty)
  | last :: before ->
      List.fold_left (fun acc arg -> mk span (combine (expr arg) acc)) (expr last) before

and cond span clauses =
  match clauses with
  | [] -> Diagnostic.error span "'cond' needs a final 'else' clause"
  | [ { node = List ({ node = Sym "else"; _ } :: (_ :: _ as body)); span = s } ] ->
      parse_body s body
  | { node = List ({ node = Sym "else"; _ } :: _); span = s } :: _ ->
      Diagnostic.error s "the 'else' clause must come last and have a body"
  | { node = List (test :: (_ :: _ as body)); span = s } :: rest ->
      if rest = [] then Diagnostic.error span "'cond' needs a final 'else' clause"
      else mk span (If (expr test, parse_body s body, cond span rest))
  | { span = s; _ } :: _ -> Diagnostic.error s "expected a clause of the form (test body)"

(* A function body can start with local [define]s. A run of consecutive function
   definitions becomes one [letrec], so they can call each other, and a value definition
   becomes a [let] scoped over everything after it. *)
and parse_body span forms =
  let is_define (form : Reader.sexp) =
    match form.node with List ({ node = Sym "define"; _ } :: _) -> true | _ -> false
  in
  let rec take_functions acc = function
    | form :: rest when is_define form -> (
        match parse_define form with
        | (_, { desc = Lambda _; _ }) as def -> take_functions (def :: acc) rest
        | _ -> (List.rev acc, form :: rest))
    | rest -> (List.rev acc, rest)
  in
  let rec go = function
    | [] ->
        Diagnostic.error span "this body must end with an expression, not a definition"
    | [ form ] when not (is_define form) -> [ expr form ]
    | form :: rest when is_define form -> (
        match take_functions [] (form :: rest) with
        | [], _ ->
            let binding = parse_define form in
            let body = sequence (go rest) in
            [ mk form.span (Let ([ binding ], body)) ]
        | functions, rest ->
            check_distinct (List.map fst functions);
            let body = sequence (go rest) in
            [ mk form.span (LetRec (functions, body)) ])
    | form :: rest -> expr form :: go rest
  and sequence = function [ single ] -> single | items -> mk span (Seq items) in
  sequence (go forms)

and parse_define (form : Reader.sexp) =
  let defined_name (name : Reader.sexp) =
    let b = binder name in
    if b.name = "_" then Diagnostic.error name.span "'_' cannot be defined";
    b
  in
  match form.node with
  | List
      ({ node = Sym "define"; _ }
      :: { node = List (name :: params); _ }
      :: (_ :: _ as body)) ->
      let params = List.map binder params in
      check_distinct params;
      (defined_name name, mk form.span (Lambda (params, parse_body form.span body)))
  | List [ { node = Sym "define"; _ }; ({ node = Sym _; _ } as name); value ] ->
      (defined_name name, expr value)
  | _ ->
      Diagnostic.error form.span
        "expected (define name value) or (define (name params...) body...)"

(* Top-level forms *)

let parse_ctor (form : Reader.sexp) =
  match form.node with
  | Sym _ -> { ctor_name = (binder form).name; ctor_span = form.span; fields = [] }
  | List (({ node = Sym _; _ } as name) :: fields) ->
      {
        ctor_name = (binder name).name;
        ctor_span = form.span;
        fields = List.map ty_expr fields;
      }
  | _ -> Diagnostic.error form.span "expected a constructor, such as (none) or (some a)"

let parse_type form_span head ctors =
  let type_binder (b : Reader.sexp) =
    let result = binder b in
    if is_type_var result.name then
      Diagnostic.errorf b.span "'%s' looks like a type variable and cannot name a type"
        result.name;
    result
  in
  let param (p : Reader.sexp) =
    let result = binder p in
    if not (is_type_var result.name) then
      Diagnostic.errorf p.span
        "'%s' is not a valid type variable (use a lowercase letter, like a or t1)"
        result.name;
    result
  in
  let name, params =
    match (head : Reader.sexp).node with
    | Sym _ -> (type_binder head, [])
    | List (first :: params) when params <> [] ->
        (type_binder first, List.map param params)
    | _ -> Diagnostic.error head.span "expected a type name, such as color or (tree a)"
  in
  check_distinct params;
  if ctors = [] then
    Diagnostic.errorf form_span "type '%s' needs at least one constructor" name.name;
  {
    type_name = name.name;
    type_span = name.binder_span;
    params;
    ctors = List.map parse_ctor ctors;
  }

let top (form : Reader.sexp) =
  match form.node with
  | List ({ node = Sym "define"; _ } :: _) ->
      let name, value = parse_define form in
      Define (name, value)
  | List [ { node = Sym "sig"; _ }; name; ty ] -> Sig (binder name, ty_expr ty)
  | List ({ node = Sym "sig"; _ } :: _) ->
      Diagnostic.error form.span "expected (sig name type)"
  | List ({ node = Sym "type"; _ } :: head :: ctors) ->
      Type (parse_type form.span head ctors)
  | List ({ node = Sym "type"; _ } :: _) ->
      Diagnostic.error form.span "expected (type name constructors...)"
  | _ -> Expr (expr form)

let program forms = List.map top forms

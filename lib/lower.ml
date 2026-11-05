open Core
module SMap = Map.Make (String)

type globals = { slots : int SMap.t; next : int }

let no_globals = { slots = SMap.empty; next = 0 }
let global_count g = g.next

type env = { locals : var SMap.t; globals : int SMap.t }

let bind_local env name var =
  if name = "_" then env else { env with locals = SMap.add name var env.locals }

let ctor_desc (c : Types.ctor) : Bytecode.ctor_desc =
  { name = c.ctor_name; tag = c.tag; is_list = c.owner.tycon.tc_id = Types.tc_list.tc_id }

let is_bool (c : Types.ctor) = c.owner.tycon.tc_id = Types.tc_bool.tc_id
let has_one_ctor (c : Types.ctor) = List.compare_length_with c.owner.ctors 1 = 0
let nil_const = Const (Data (ctor_desc Types.ctor_nil, [||]))
let cons_desc = ctor_desc Types.ctor_cons

(* Wraps a primitive or constructor as a function, for when it is used as a value. *)
let eta name arity make =
  let params = List.init arity (fun _ -> fresh "arg") in
  Lambda { fn_name = name; params; body = make (List.map (fun p -> Local p) params) }

(* Whether [Fail] can escape this expression. Only [Fail] that isn't caught by an enclosing
   [Fatbar] counts. Clause bodies never contain an escaping one, since each nested match is
   closed off on its own. *)
let rec can_fail = function
  | Fail -> true
  | Fatbar (_, b) -> can_fail b
  | If (_, a, b) -> can_fail a || can_fail b
  | Let (_, _, body) -> can_fail body
  | Switch (_, cases, n) ->
      List.compare_length_with cases n < 0
      || List.exists (fun (_, body) -> can_fail body) cases
  | _ -> false

let fatbar a b = if can_fail a then Fatbar (a, b) else a

(* Pattern-match compilation.

   A clause set is a matrix: one row per clause, one column per value being examined. Each step
   looks at the first column, following the approach in chapter 5 of Peyton Jones, "The
   Implementation of Functional Programming Languages". *)

type row = {
  patterns : Typed.pattern list;
  bindings : (string * var) list;
  action : Typed.expr;
}

type head_kind = Wild | Ctor | Literal

let head_kind row =
  match row.patterns with
  | (PWild | PVar _) :: _ -> Wild
  | PCtor _ :: _ -> Ctor
  | (PInt _ | PStr _) :: _ -> Literal
  | [] -> assert false

(* Variables just bind the value being examined, and the single value of a one-constructor,
   no-field type needs no test, so both act like wildcards. *)
let simplify_head var row =
  match row.patterns with
  | PVar name :: rest ->
      { row with patterns = PWild :: rest; bindings = (name, var) :: row.bindings }
  | PCtor (c, []) :: rest when has_one_ctor c -> { row with patterns = PWild :: rest }
  | _ -> row

(* Splits rows into maximal runs with the same kind of head. Rows must stay in order, because an
   earlier clause takes priority, so unlike kinds can't be merged. *)
let rec group_runs = function
  | [] -> []
  | row :: rest -> (
      let kind = head_kind row in
      match group_runs rest with
      | (k, run) :: others when k = kind -> (k, row :: run) :: others
      | groups -> (kind, [ row ]) :: groups)

let tail_patterns row =
  match row.patterns with _ :: rest -> { row with patterns = rest } | [] -> assert false

let same_literal (a : Typed.pattern) (b : Typed.pattern) =
  match (a, b) with
  | PInt x, PInt y -> Bigint.equal x y
  | PStr x, PStr y -> String.equal x y
  | _ -> false

let rec lower env (e : Typed.expr) : Core.expr =
  match e.desc with
  | Int n -> Const (Value.of_bigint n)
  | Str s -> Const (Str s)
  | Bool b -> Const (if b then True else False)
  | Unit -> Const Unit
  | Var name -> lower_var env name e.span
  | CtorApp (c, []) -> Const (Data (ctor_desc c, [||]))
  | CtorApp (c, args) -> MakeData (ctor_desc c, List.map (lower env) args)
  | CtorFn c ->
      eta c.ctor_name (List.length c.fields) (fun args -> MakeData (ctor_desc c, args))
  | ListLit items ->
      List.fold_right
        (fun item rest -> MakeData (cons_desc, [ lower env item; rest ]))
        items nil_const
  | Lambda _ -> lower_named env "<lambda>" e
  | App ({ desc = Var name; _ }, args) when is_primitive env name ->
      let builtin = Option.get (Builtins.find name) in
      Prim (builtin.instr, List.map (lower env) args, e.span)
  | App (f, args) -> Call (lower env f, List.map (lower env) args, e.span)
  | If (c, t, f) -> If (lower env c, lower env t, lower env f)
  | Let (bindings, body) ->
      (* Right-hand sides are lowered in the outer scope, then bound one at a time. Unique
         variables make the nesting equivalent to binding them all at once. *)
      let bindings =
        List.map
          (fun (name, rhs) -> (name, fresh name, lower_named env name rhs))
          bindings
      in
      let inner =
        List.fold_left (fun env (name, var, _) -> bind_local env name var) env bindings
      in
      List.fold_right
        (fun (_, var, rhs) rest -> Let (var, rhs, rest))
        bindings (lower inner body)
  | LetRec (bindings, body) ->
      let vars = List.map (fun (name, _) -> (name, fresh name)) bindings in
      let inner =
        List.fold_left (fun env (name, var) -> bind_local env name var) env vars
      in
      let lambdas =
        List.map2
          (fun (name, rhs) (_, var) -> (var, lower_lambda inner name rhs))
          bindings vars
      in
      LetRec (lambdas, lower inner body)
  | Match (scrutinee, clauses) -> lower_match env e.span scrutinee clauses
  | Seq items -> (
      match List.rev_map (lower env) items with
      | last :: before -> List.fold_left (fun rest item -> Seq (item, rest)) last before
      | [] -> assert false)

(* A name refers to a primitive only if nothing in scope shadows it. *)
and is_primitive env name =
  (not (SMap.mem name env.locals))
  && (not (SMap.mem name env.globals))
  && Option.is_some (Builtins.find name)

and lower_var env name span =
  match SMap.find_opt name env.locals with
  | Some var -> Local var
  | None -> (
      match SMap.find_opt name env.globals with
      | Some slot -> Global (slot, name, span)
      | None -> (
          match Builtins.find name with
          | Some b -> eta b.name b.arity (fun args -> Prim (b.instr, args, span))
          | None -> failwith ("lower: unresolved name " ^ name)))

(* Lowers a right-hand side, naming it if it is a lambda so stack traces can show the name. *)
and lower_named env name (e : Typed.expr) =
  match e.desc with Lambda _ -> Lambda (lower_lambda env name e) | _ -> lower env e

and lower_lambda env name (e : Typed.expr) =
  match e.desc with
  | Lambda (params, body) ->
      let vars = List.map (fun p -> (p, fresh p)) params in
      let inner = List.fold_left (fun env (p, v) -> bind_local env p v) env vars in
      { fn_name = name; params = List.map snd vars; body = lower inner body }
  | _ -> failwith "lower: expected a lambda"

and lower_match env span scrutinee clauses =
  let rows =
    List.map (fun (p, action) -> { patterns = [ p ]; bindings = []; action }) clauses
  in
  let compile var =
    let tree = match_rows env [ var ] rows in
    fatbar tree (MatchFailure span)
  in
  match lower env scrutinee with
  | Local var -> compile var
  | value ->
      let var = fresh "scrutinee" in
      Let (var, value, compile var)

and match_rows env vars rows =
  match (vars, rows) with
  | _, [] -> Fail
  | [], row :: _ ->
      (* No columns are left to test, so the first remaining clause has matched. Earlier rows
         take priority, and later ones are unreachable from here. *)
      let inner =
        List.fold_left (fun env (name, var) -> bind_local env name var) env row.bindings
      in
      lower inner row.action
  | var :: rest_vars, _ ->
      let rows = List.map (simplify_head var) rows in
      let compile_run (kind, run) =
        match kind with
        | Wild -> match_rows env rest_vars (List.map tail_patterns run)
        | Ctor -> match_ctors env var rest_vars run
        | Literal -> match_literals env var rest_vars run
      in
      let rec combine = function
        | [] -> Fail
        | [ run ] -> compile_run run
        | run :: rest -> fatbar (compile_run run) (combine rest)
      in
      combine (group_runs rows)

and match_ctors env var rest_vars rows =
  let head_ctor row =
    match row.patterns with PCtor (c, _) :: _ -> c | _ -> assert false
  in
  let same_ctor (a : Types.ctor) (b : Types.ctor) = a.tag = b.tag in
  let distinct =
    List.fold_left
      (fun seen row ->
        let c = head_ctor row in
        if List.exists (same_ctor c) seen then seen else seen @ [ c ])
      [] rows
  in
  let compile_case (c : Types.ctor) =
    let matching = List.filter (fun row -> same_ctor (head_ctor row) c) rows in
    let field_patterns row =
      match row.patterns with PCtor (_, args) :: _ -> args | _ -> assert false
    in
    (* A field that every row ignores never needs to be loaded. *)
    let needed =
      List.init (List.length c.fields) Fun.id
      |> List.filter (fun i ->
          List.exists
            (fun row ->
              match List.nth (field_patterns row) i with
              | Typed.PWild -> false
              | _ -> true)
            matching)
      |> List.map (fun i -> (i, fresh "field"))
    in
    let specialized =
      List.map
        (fun row ->
          let args = field_patterns row in
          let kept = List.map (fun (i, _) -> List.nth args i) needed in
          { (tail_patterns row) with patterns = kept @ List.tl row.patterns })
        matching
    in
    let body = match_rows env (List.map snd needed @ rest_vars) specialized in
    List.fold_right
      (fun (i, field_var) rest -> Let (field_var, Field (var, i), rest))
      needed body
  in
  match distinct with
  | c :: _ when is_bool c ->
      let case tag =
        match List.find_opt (fun (d : Types.ctor) -> d.tag = tag) distinct with
        | Some d -> compile_case d
        | None -> Fail
      in
      If (Local var, case Types.ctor_true.tag, case Types.ctor_false.tag)
  | [ c ] when has_one_ctor c -> compile_case c
  | c :: _ ->
      let cases = List.map (fun (d : Types.ctor) -> (d.tag, compile_case d)) distinct in
      Switch (var, cases, List.length c.owner.ctors)
  | [] -> assert false

and match_literals env var rest_vars rows =
  let head row = List.hd row.patterns in
  let rec go = function
    | [] -> Fail
    | row :: _ as rows ->
        let literal = head row in
        let matching, others =
          List.partition (fun r -> same_literal (head r) literal) rows
        in
        let constant : Core.expr =
          match literal with
          | PInt n -> Const (Value.of_bigint n)
          | PStr s -> Const (Str s)
          | _ -> assert false
        in
        let test = Prim (Eq, [ Local var; constant ], Diagnostic.none) in
        If (test, match_rows env rest_vars (List.map tail_patterns matching), go others)
  in
  go rows

(* Whole units *)

let program globals (items : Typed.top list) =
  let slots, next =
    List.fold_left
      (fun (slots, next) -> function
        | Typed.Define (name, _) -> (SMap.add name next slots, next + 1)
        | Typed.Expr _ -> (slots, next))
      (globals.slots, globals.next) items
  in
  let env = { locals = SMap.empty; globals = slots } in
  let define name rhs = SetGlobal (SMap.find name slots, lower_named env name rhs) in
  let is_function : Typed.top -> bool = function
    | Define (_, { desc = Lambda _; _ }) -> true
    | _ -> false
  in
  let functions, others = List.partition is_function items in
  let lowered_functions =
    List.map
      (function
        | Typed.Define (name, rhs) -> define name rhs | Typed.Expr _ -> assert false)
      functions
  in
  let lowered_others =
    List.map
      (function
        | Typed.Define (name, rhs) -> define name rhs | Typed.Expr e -> lower env e)
      others
  in
  (* The unit's value is its last form's, if that form is an expression. Hoisting moves only
     function definitions, so the last of [others] is still the last value computed. *)
  let ends_in_expr = match List.rev items with Typed.Expr _ :: _ -> true | _ -> false in
  let steps = lowered_functions @ lowered_others in
  let body =
    match (List.rev steps, ends_in_expr) with
    | [], _ -> Const Unit
    | last :: before, true ->
        List.fold_left (fun rest step -> Seq (step, rest)) last before
    | reversed, false ->
        List.fold_left (fun rest step -> Seq (step, rest)) (Const Unit) reversed
  in
  ({ slots; next }, body)

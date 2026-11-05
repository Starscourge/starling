open Types
module SMap = Map.Make (String)
module SSet = Set.Make (String)

type binding = Value of scheme | Constructor of ctor

type env = {
  values : binding SMap.t;
      (** Variables and constructors share a namespace in expressions. *)
  ctors : ctor SMap.t;
      (** Patterns only see constructors, so variables can't shadow them. *)
  types : data_type SMap.t;
}

let initial_env =
  let add_ctor env (c : ctor) =
    {
      env with
      values = SMap.add c.ctor_name (Constructor c) env.values;
      ctors = SMap.add c.ctor_name c env.ctors;
    }
  in
  let add_type env (dt : data_type) =
    { env with types = SMap.add dt.tycon.tc_name dt env.types }
  in
  let env = { values = SMap.empty; ctors = SMap.empty; types = SMap.empty } in
  let env = List.fold_left add_type env builtin_types in
  (* The reserved words true, false and unit have literal syntax of their own, so they are
     patterns but not ordinary values. *)
  let env =
    List.fold_left
      (fun env (c : ctor) -> { env with ctors = SMap.add c.ctor_name c env.ctors })
      env
      [ ctor_true; ctor_false; ctor_unit ]
  in
  let env = List.fold_left add_ctor env [ ctor_nil; ctor_cons ] in
  List.fold_left
    (fun env (b : Builtins.t) ->
      { env with values = SMap.add b.name (Value b.scheme) env.values })
    env Builtins.all

(* Levels *)

let current_level = ref 0
let fresh () = TVar (ref (Unbound { id = next_id (); level = !current_level }))

let with_deeper_level f =
  incr current_level;
  match f () with
  | result ->
      decr current_level;
      result
  | exception e ->
      decr current_level;
      raise e

(* Quantifies the variables created deeper than the current level. *)
let generalize ty =
  let vars = ref [] and count = ref 0 in
  let rec go t =
    match repr t with
    | TVar r as var -> (
        match !r with
        | Unbound { level; _ } when level > !current_level -> (
            match List.assq_opt r !vars with
            | Some gen -> gen
            | None ->
                let gen = TGen !count in
                incr count;
                vars := (r, gen) :: !vars;
                gen)
        | _ -> var)
    | TCon (c, args) -> TCon (c, List.map go args)
    | TFun (params, result) ->
        let params = List.map go params in
        TFun (params, go result)
    | (TGen _ | TRigid _) as t -> t
  in
  let body = go ty in
  { quantified = !count; body }

let instantiate { quantified; body } =
  if quantified = 0 then body
  else subst_gen (Array.init quantified (fun _ -> fresh ())) body

(* Replaces quantified variables with rigid ones, to check a definition against its
   signature. A rigid variable unifies only with itself, so a body that isn't really that
   general gets rejected. *)
let skolemize names { quantified; body } =
  let rigid i = TRigid (next_id (), List.nth names i) in
  subst_gen (Array.init quantified rigid) body

(* Unification *)

exception Mismatch of ty * ty
exception Occurs of ty * ty

(* Binding a variable to [ty] requires checking two things in one pass. The variable must not
   occur inside [ty], since that would make an infinite type. And every variable in [ty] must
   be lowered to the variable's level, because it is now reachable from that scope and must
   not be generalized any deeper. *)
let bind var ({ contents = binding } as r) ty =
  let limit =
    match binding with Unbound { level; _ } -> level | Link _ -> assert false
  in
  let rec walk t =
    match repr t with
    | TVar r' when r' == r -> raise (Occurs (var, ty))
    | TVar ({ contents = Unbound { id; level } } as r') ->
        if level > limit then r' := Unbound { id; level = limit }
    | TVar _ -> ()
    | TCon (_, args) -> List.iter walk args
    | TFun (params, result) ->
        List.iter walk params;
        walk result
    | TGen _ | TRigid _ -> ()
  in
  walk ty;
  r := Link ty

let rec unify t1 t2 =
  let t1 = repr t1 and t2 = repr t2 in
  if t1 != t2 then
    match (t1, t2) with
    | TVar r, t | t, TVar r ->
        let var = if repr t1 == t then t2 else t1 in
        bind var r t
    | TCon (c1, args1), TCon (c2, args2) when c1.tc_id = c2.tc_id ->
        List.iter2 unify args1 args2
    | TFun (ps1, r1), TFun (ps2, r2) when List.compare_lengths ps1 ps2 = 0 ->
        List.iter2 unify ps1 ps2;
        unify r1 r2
    | TRigid (i, _), TRigid (j, _) when i = j -> ()
    | _ -> raise (Mismatch (t1, t2))

let rigid_note t1 t2 =
  let note = function
    | TRigid (_, name) ->
        [
          Printf.sprintf
            "'%s' comes from a type signature, so the definition must work for every type"
            name;
        ]
    | _ -> []
  in
  note t1 @ note t2

let expect span ~expected ~found =
  try unify expected found with
  | Mismatch (t1, t2) ->
      let show = printer () in
      let expected = show expected in
      let found = show found in
      Diagnostic.errorf span ~notes:(rigid_note t1 t2)
        "type mismatch: expected %s, found %s" expected found
  | Occurs (var, ty) ->
      let show = printer () in
      let var = show var in
      Diagnostic.errorf span "infinite type: %s would have to contain itself in %s" var
        (show ty)

(* Type expressions *)

(* [on_var] decides what a type variable means in this context. *)
let rec resolve_type env ~on_var (t : Syntax.ty_expr) =
  let span = t.ty_span in
  let lookup name =
    match SMap.find_opt name env.types with
    | Some dt -> dt.tycon
    | None -> Diagnostic.errorf span "unknown type '%s'" name
  in
  let check_arity (c : tycon) n =
    if c.tc_arity <> n then
      Diagnostic.errorf span "type '%s' takes %d argument(s), but got %d" c.tc_name
        c.tc_arity n
  in
  match t.ty with
  | TyName name when Parse.is_type_var name -> on_var span name
  | TyName name ->
      let c = lookup name in
      check_arity c 0;
      TCon (c, [])
  | TyApp (name, args) ->
      let c = lookup name in
      check_arity c (List.length args);
      TCon (c, List.map (resolve_type env ~on_var) args)
  | TyFun (params, result) ->
      let params = List.map (resolve_type env ~on_var) params in
      TFun (params, resolve_type env ~on_var result)

(* Each distinct variable name gets one meaning, produced on first use by [make]. *)
let memoized_vars make =
  let seen = ref [] in
  let on_var _span name =
    match List.assoc_opt name !seen with
    | Some t -> t
    | None ->
        let t = make (List.length !seen) name in
        seen := (name, t) :: !seen;
        t
  in
  (on_var, seen)

(* Suggestions for misspelled names *)

let edit_distance a b =
  let la = String.length a and lb = String.length b in
  let row = Array.init (lb + 1) Fun.id in
  for i = 1 to la do
    let diagonal = ref row.(0) in
    row.(0) <- i;
    for j = 1 to lb do
      let above = row.(j) in
      let cost = if a.[i - 1] = b.[j - 1] then 0 else 1 in
      row.(j) <- min (min (above + 1) (row.(j - 1) + 1)) (!diagonal + cost);
      diagonal := above
    done
  done;
  row.(lb)

let suggestion env name =
  let consider best candidate _ =
    let d = edit_distance name candidate in
    match best with
    | Some (best_d, _) when best_d <= d -> best
    | _ when d <= 2 && d < String.length name -> Some (d, candidate)
    | _ -> best
  in
  match SMap.fold (fun k v best -> consider best k v) env.values None with
  | Some (_, candidate) -> [ "did you mean '" ^ candidate ^ "'?" ]
  | None -> []

(* Patterns *)

let rec infer_pattern env bound expected (p : Syntax.pattern) : Typed.pattern =
  let span = p.pat_span in
  match p.pat with
  | PWild -> PWild
  | PInt n ->
      expect span ~expected ~found:t_int;
      PInt n
  | PStr s ->
      expect span ~expected ~found:t_string;
      PStr s
  | PIdent name -> (
      match SMap.find_opt name env.ctors with
      | Some c when c.fields = [] -> infer_ctor_pattern env bound expected span c []
      | _ ->
          if List.mem_assoc name !bound then
            Diagnostic.errorf span "'%s' is bound more than once in this pattern" name;
          bound := (name, expected) :: !bound;
          PVar name)
  | PCtor (name, args) -> (
      match SMap.find_opt name env.ctors with
      | Some c -> infer_ctor_pattern env bound expected span c args
      | None -> Diagnostic.errorf span "unknown constructor '%s'" name)

and infer_ctor_pattern env bound expected span (c : ctor) args =
  let n_fields = List.length c.fields in
  if List.compare_length_with args n_fields <> 0 then
    Diagnostic.errorf span
      "constructor '%s' takes %d argument(s), but this pattern has %d" c.ctor_name
      n_fields (List.length args);
  let type_args = Array.init c.owner.tycon.tc_arity (fun _ -> fresh ()) in
  expect span ~expected ~found:(TCon (c.owner.tycon, Array.to_list type_args));
  let check_field field arg = infer_pattern env bound (subst_gen type_args field) arg in
  PCtor (c, List.map2 check_field c.fields args)

(* Expressions *)

let bind_value env name scheme =
  if name = "_" then env
  else { env with values = SMap.add name (Value scheme) env.values }

let rec infer env (e : Syntax.expr) : Typed.expr * ty =
  let mk desc : Typed.expr = { desc; span = e.span } in
  match e.desc with
  | Int n -> (mk (Int n), t_int)
  | Str s -> (mk (Str s), t_string)
  | Bool b -> (mk (Bool b), t_bool)
  | Unit -> (mk Unit, t_unit)
  | Var name -> (
      match SMap.find_opt name env.values with
      | Some (Value scheme) -> (mk (Var name), instantiate scheme)
      | Some (Constructor c) -> constructor_value e c
      | None ->
          Diagnostic.errorf e.span ~notes:(suggestion env name) "unbound variable '%s'"
            name)
  | ListLit items ->
      let element = fresh () in
      let items = List.map (fun item -> check env item element) items in
      (mk (ListLit items), t_list element)
  | Lambda (params, body) ->
      let param_types = List.map (fun _ -> fresh ()) params in
      let body_env =
        List.fold_left2
          (fun env (p : Syntax.binder) t -> bind_value env p.name (mono t))
          env params param_types
      in
      let body, result = infer body_env body in
      let names = List.map (fun (p : Syntax.binder) -> p.name) params in
      (mk (Lambda (names, body)), TFun (param_types, result))
  | App (f, args) -> (
      match f.desc with
      | Var name -> (
          match SMap.find_opt name env.values with
          | Some (Constructor c) -> constructor_application env e c args
          | _ -> infer_application env e f args)
      | _ -> infer_application env e f args)
  | If (c, t, f) ->
      let c = check env c t_bool in
      let t, result = infer env t in
      let f' = check env f result in
      (mk (If (c, t, f')), result)
  | Let (bindings, body) ->
      let bindings = List.map (fun (b, rhs) -> (b, infer_generalized env rhs)) bindings in
      let body_env =
        List.fold_left
          (fun inner ((b : Syntax.binder), (_, scheme)) -> bind_value inner b.name scheme)
          env bindings
      in
      let body, result = infer body_env body in
      let names =
        List.map (fun ((b : Syntax.binder), (rhs, _)) -> (b.name, rhs)) bindings
      in
      (mk (Let (names, body)), result)
  | LetRec (bindings, body) ->
      let inner_env, checked = infer_recursive env bindings in
      let body, result = infer inner_env body in
      (mk (LetRec (checked, body)), result)
  | Match (scrutinee, clauses) -> infer_match env e scrutinee clauses
  | Seq items ->
      let rec go acc = function
        | [] -> assert false
        | [ last ] ->
            let last, t = infer env last in
            (List.rev (last :: acc), t)
        | item :: rest ->
            let item, _ = infer env item in
            go (item :: acc) rest
      in
      let items, t = go [] items in
      (mk (Seq items), t)
  | The (ty, inner) ->
      (* Variables in an annotation here are flexible: (the (list a) x) asserts only that x
         is some list. *)
      let on_var, _ = memoized_vars (fun _ _ -> fresh ()) in
      let annotated = resolve_type env ~on_var ty in
      (check env inner annotated, annotated)

and check env (e : Syntax.expr) expected =
  let typed, found = infer env e in
  expect e.span ~expected ~found;
  typed

and infer_generalized env rhs =
  let rhs, t = with_deeper_level (fun () -> infer env rhs) in
  (rhs, generalize t)

and constructor_value (e : Syntax.expr) (c : ctor) =
  let type_args = Array.init c.owner.tycon.tc_arity (fun _ -> fresh ()) in
  let result = TCon (c.owner.tycon, Array.to_list type_args) in
  let mk desc : Typed.expr = { desc; span = e.span } in
  match c.fields with
  | [] -> (mk (CtorApp (c, [])), result)
  | fields -> (mk (CtorFn c), TFun (List.map (subst_gen type_args) fields, result))

and constructor_application env (e : Syntax.expr) (c : ctor) args =
  let n_fields = List.length c.fields in
  if List.compare_length_with args n_fields <> 0 then
    Diagnostic.errorf e.span "constructor '%s' takes %d argument(s), but got %d"
      c.ctor_name n_fields (List.length args);
  let type_args = Array.init c.owner.tycon.tc_arity (fun _ -> fresh ()) in
  let args =
    List.map2 (fun field arg -> check env arg (subst_gen type_args field)) c.fields args
  in
  let typed : Typed.expr = { desc = CtorApp (c, args); span = e.span } in
  (typed, TCon (c.owner.tycon, Array.to_list type_args))

and infer_application env (e : Syntax.expr) f args =
  let typed_f, f_type = infer env f in
  let n_args = List.length args in
  let params, result =
    match repr f_type with
    | TFun (params, result) ->
        let n_params = List.length params in
        if n_params <> n_args then
          Diagnostic.errorf e.span "this function takes %d argument(s), but got %d"
            n_params n_args;
        (params, result)
    | TVar _ as var ->
        let params = List.map (fun _ -> fresh ()) args in
        let result = fresh () in
        expect f.span ~expected:var ~found:(TFun (params, result));
        (params, result)
    | other ->
        Diagnostic.errorf f.span "this is not a function, it has type %s"
          (to_string other)
  in
  let args = List.map2 (fun arg param -> check env arg param) args params in
  ({ desc = App (typed_f, args); span = e.span }, result)

and infer_recursive env bindings =
  let inferred =
    with_deeper_level (fun () ->
        let assumed = List.map (fun _ -> fresh ()) bindings in
        let rec_env =
          List.fold_left2
            (fun env ((b : Syntax.binder), _) t -> bind_value env b.name (mono t))
            env bindings assumed
        in
        List.map2
          (fun ((b : Syntax.binder), rhs) t ->
            let rhs = check rec_env rhs t in
            (b.name, rhs, t))
          bindings assumed)
  in
  let inner_env =
    List.fold_left
      (fun env (name, _, t) -> bind_value env name (generalize t))
      env inferred
  in
  (inner_env, List.map (fun (name, rhs, _) -> (name, rhs)) inferred)

and infer_match env (e : Syntax.expr) scrutinee clauses =
  let scrutinee, scrutinee_type = infer env scrutinee in
  let result = fresh () in
  let checked =
    List.map
      (fun ((p : Syntax.pattern), (body : Syntax.expr)) ->
        let bound = ref [] in
        let pattern = infer_pattern env bound scrutinee_type p in
        let clause_env =
          List.fold_left (fun env (name, t) -> bind_value env name (mono t)) env !bound
        in
        let body = check clause_env body result in
        (pattern, p.pat_span, body))
      clauses
  in
  Exhaustive.check e.span (List.map (fun (p, span, _) -> (p, span)) checked);
  let clauses = List.map (fun (p, _, body) -> (p, body)) checked in
  ({ desc = Match (scrutinee, clauses); span = e.span }, result)

(* Top level: dependency analysis *)

(* The free variables of an expression. A nullary constructor in a pattern counts as a binder
   here, which is harmless: it can only hide a dependency on a global of the same name, and
   that global isn't what the pattern refers to anyway. *)
let rec free_vars bound (e : Syntax.expr) acc =
  let params_of = List.map (fun (b : Syntax.binder) -> b.name) in
  let extend names = SSet.union bound (SSet.of_list names) in
  match e.desc with
  | Int _ | Str _ | Bool _ | Unit -> acc
  | Var name -> if SSet.mem name bound then acc else SSet.add name acc
  | ListLit items | Seq items ->
      List.fold_left (fun acc item -> free_vars bound item acc) acc items
  | Lambda (params, body) -> free_vars (extend (params_of params)) body acc
  | App (f, args) ->
      List.fold_left (fun acc arg -> free_vars bound arg acc) (free_vars bound f acc) args
  | If (a, b, c) -> free_vars bound a (free_vars bound b (free_vars bound c acc))
  | Let (bindings, body) ->
      let acc =
        List.fold_left (fun acc (_, rhs) -> free_vars bound rhs acc) acc bindings
      in
      free_vars (extend (params_of (List.map fst bindings))) body acc
  | LetRec (bindings, body) ->
      let inner = extend (params_of (List.map fst bindings)) in
      let acc =
        List.fold_left (fun acc (_, rhs) -> free_vars inner rhs acc) acc bindings
      in
      free_vars inner body acc
  | Match (scrutinee, clauses) ->
      let acc = free_vars bound scrutinee acc in
      List.fold_left
        (fun acc (p, body) -> free_vars (extend (pattern_vars p [])) body acc)
        acc clauses
  | The (_, inner) -> free_vars bound inner acc

and pattern_vars (p : Syntax.pattern) acc =
  match p.pat with
  | PWild | PInt _ | PStr _ -> acc
  | PIdent name -> name :: acc
  | PCtor (_, args) -> List.fold_left (fun acc arg -> pattern_vars arg acc) acc args

(* Tarjan's algorithm. Components come out dependencies-first, which is the order to check
   them in. Grouping this way keeps definitions as polymorphic as possible: a function is
   monomorphic only among the functions it is genuinely mutually recursive with. *)
let strongly_connected_components n successors =
  let index = Array.make n (-1) and low = Array.make n 0 in
  let on_stack = Array.make n false in
  let stack = ref [] and counter = ref 0 and result = ref [] in
  let rec visit v =
    index.(v) <- !counter;
    low.(v) <- !counter;
    incr counter;
    stack := v :: !stack;
    on_stack.(v) <- true;
    List.iter
      (fun w ->
        if index.(w) < 0 then (
          visit w;
          low.(v) <- min low.(v) low.(w))
        else if on_stack.(w) then low.(v) <- min low.(v) index.(w))
      (successors v);
    if low.(v) = index.(v) then
      let rec pop acc =
        match !stack with
        | w :: rest ->
            stack := rest;
            on_stack.(w) <- false;
            if w = v then w :: acc else pop (w :: acc)
        | [] -> assert false
      in
      result := pop [] :: !result
  in
  for v = 0 to n - 1 do
    if index.(v) < 0 then visit v
  done;
  List.rev !result

(* Top level: type declarations *)

let declare_types env (decls : Syntax.type_decl list) =
  let reject_duplicates seen name span what =
    if SSet.mem name !seen then
      Diagnostic.errorf span "%s '%s' is defined twice" what name;
    seen := SSet.add name !seen
  in
  let type_names = ref SSet.empty in
  (* First make every name known, so types in this unit can refer to each other. *)
  let declared =
    List.map
      (fun (d : Syntax.type_decl) ->
        reject_duplicates type_names d.type_name d.type_span "type";
        if
          List.exists (fun (b : data_type) -> b.tycon.tc_name = d.type_name) builtin_types
        then Diagnostic.errorf d.type_span "'%s' is a built-in type" d.type_name;
        let tycon = new_tycon d.type_name (List.length d.params) in
        (d, ({ tycon; ctors = [] } : data_type)))
      decls
  in
  let env =
    List.fold_left
      (fun env ((d : Syntax.type_decl), dt) ->
        { env with types = SMap.add d.type_name dt env.types })
      env declared
  in
  (* Then resolve the constructor fields. *)
  let ctor_names = ref SSet.empty in
  List.fold_left
    (fun env ((d : Syntax.type_decl), (dt : data_type)) ->
      let positions =
        List.mapi (fun i (p : Syntax.binder) -> (p.name, TGen i)) d.params
      in
      let on_var span name =
        match List.assoc_opt name positions with
        | Some gen -> gen
        | None ->
            Diagnostic.errorf span "type variable '%s' is not a parameter of type '%s'"
              name d.type_name
      in
      dt.ctors <-
        List.mapi
          (fun tag (c : Syntax.ctor_decl) ->
            reject_duplicates ctor_names c.ctor_name c.ctor_span "constructor";
            {
              ctor_name = c.ctor_name;
              tag;
              fields = List.map (resolve_type env ~on_var) c.fields;
              owner = dt;
            })
          d.ctors;
      List.fold_left
        (fun env (c : ctor) ->
          {
            env with
            values = SMap.add c.ctor_name (Constructor c) env.values;
            ctors = SMap.add c.ctor_name c env.ctors;
          })
        env dt.ctors)
    env declared

(* Top level: signatures *)

type signature = { scheme : scheme; var_names : string list }

let resolve_signature env ty =
  let on_var, seen = memoized_vars (fun i _name -> TGen i) in
  let body = resolve_type env ~on_var ty in
  let names = List.rev_map fst !seen in
  { scheme = { quantified = List.length names; body }; var_names = names }

(* Top level: definitions *)

let check_definitions env defines signatures =
  let defines = Array.of_list defines in
  let positions = Hashtbl.create 16 in
  Array.iteri
    (fun i ((b : Syntax.binder), _) ->
      if Hashtbl.mem positions b.name then
        Diagnostic.errorf b.binder_span "'%s' is defined twice" b.name;
      Hashtbl.add positions b.name i)
    defines;
  (* A definition with a signature can be used before its body is checked, so depending on
     one creates no ordering constraint. This is what allows polymorphic recursion. *)
  let successors v =
    let _, rhs = defines.(v) in
    SSet.fold
      (fun name acc ->
        match Hashtbl.find_opt positions name with
        | Some w when not (SMap.mem name signatures) -> w :: acc
        | _ -> acc)
      (free_vars SSet.empty rhs SSet.empty)
      []
  in
  let components = strongly_connected_components (Array.length defines) successors in
  let env = SMap.fold (fun name s env -> bind_value env name s.scheme) signatures env in
  let typed = Hashtbl.create 16 in
  let check_component env component =
    let members = List.map (fun i -> defines.(i)) component in
    match members with
    | [ ((b : Syntax.binder), rhs) ] when SMap.mem b.name signatures ->
        let s = SMap.find b.name signatures in
        let expected = skolemize s.var_names s.scheme in
        let rhs = with_deeper_level (fun () -> check env rhs expected) in
        Hashtbl.replace typed b.name rhs;
        env
    | _ ->
        let env, checked = infer_recursive env members in
        List.iter (fun (name, rhs) -> Hashtbl.replace typed name rhs) checked;
        env
  in
  let env = List.fold_left check_component env components in
  (env, fun name -> Hashtbl.find typed name)

(* Whole programs *)

type result = {
  env : env;
  items : Typed.top list;
  definitions : (string * scheme) list;
  last_type : scheme option;
}

let lookup_scheme env name =
  match SMap.find_opt name env.values with
  | Some (Value scheme) -> scheme
  | _ -> assert false

let check_program env (tops : Syntax.top list) =
  current_level := 0;
  let type_decls =
    List.filter_map (function Syntax.Type d -> Some d | _ -> None) tops
  in
  let env = declare_types env type_decls in
  let defines =
    List.filter_map (function Syntax.Define (b, rhs) -> Some (b, rhs) | _ -> None) tops
  in
  let signatures =
    List.fold_left
      (fun acc -> function
        | Syntax.Sig (b, ty) ->
            if SMap.mem b.name acc then
              Diagnostic.errorf b.binder_span "'%s' has more than one signature" b.name;
            if not (List.exists (fun ((d : Syntax.binder), _) -> d.name = b.name) defines)
            then
              Diagnostic.errorf b.binder_span "signature for '%s' has no definition"
                b.name;
            SMap.add b.name (resolve_signature env ty) acc
        | _ -> acc)
      SMap.empty tops
  in
  let env, typed_rhs = check_definitions env defines signatures in
  (* Expressions are checked after every definition, since functions are visible throughout
     the file regardless of order. *)
  let last_type = ref None in
  let items =
    List.filter_map
      (function
        | Syntax.Define (b, _) -> Some (Typed.Define (b.name, typed_rhs b.name))
        | Syntax.Expr e ->
            let typed, scheme = infer_generalized env e in
            last_type := Some scheme;
            Some (Typed.Expr typed)
        | Syntax.Type _ | Syntax.Sig _ -> None)
      tops
  in
  let last_type =
    match List.rev tops with Syntax.Expr _ :: _ -> !last_type | _ -> None
  in
  let definitions =
    List.map (fun ((b : Syntax.binder), _) -> (b.name, lookup_scheme env b.name)) defines
  in
  { env; items; definitions; last_type }

let type_of_expr env e =
  current_level := 0;
  snd (infer_generalized env e)

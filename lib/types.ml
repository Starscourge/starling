type tycon = { tc_name : string; tc_id : int; tc_arity : int }

type ty =
  | TVar of tvar ref
  | TCon of tycon * ty list
  | TFun of ty list * ty
  | TGen of int
  | TRigid of int * string

and tvar = Unbound of { id : int; level : int } | Link of ty

type scheme = { quantified : int; body : ty }

type ctor = { ctor_name : string; tag : int; fields : ty list; owner : data_type }
and data_type = { tycon : tycon; mutable ctors : ctor list }

let mono body = { quantified = 0; body }

let rec repr = function
  | TVar ({ contents = Link t } as r) ->
      let t = repr t in
      r := Link t;
      t
  | t -> t

let rec subst_gen args t =
  match repr t with
  | TGen i -> args.(i)
  | TCon (c, ts) -> TCon (c, List.map (subst_gen args) ts)
  | TFun (ps, r) -> TFun (List.map (subst_gen args) ps, subst_gen args r)
  | (TVar _ | TRigid _) as t -> t

let counter = ref 0

let next_id () =
  incr counter;
  !counter

let new_tycon tc_name tc_arity = { tc_name; tc_id = next_id (); tc_arity }

(* Built-ins *)

let tc_int = new_tycon "int" 0
let tc_string = new_tycon "string" 0
let tc_bool = new_tycon "bool" 0
let tc_unit = new_tycon "unit" 0
let tc_list = new_tycon "list" 1
let t_int = TCon (tc_int, [])
let t_string = TCon (tc_string, [])
let t_bool = TCon (tc_bool, [])
let t_unit = TCon (tc_unit, [])
let t_list t = TCon (tc_list, [ t ])

(* Builds a data type and its constructors in one step, because each refers to the other. *)
let data_type tycon specs =
  let owner = { tycon; ctors = [] } in
  owner.ctors <-
    List.mapi (fun tag (ctor_name, fields) -> { ctor_name; tag; fields; owner }) specs;
  owner

let dt_bool = data_type tc_bool [ ("false", []); ("true", []) ]
let dt_unit = data_type tc_unit [ ("unit", []) ]

let dt_list =
  data_type tc_list [ ("nil", []); ("cons", [ TGen 0; TCon (tc_list, [ TGen 0 ]) ]) ]

let builtin_types =
  [
    { tycon = tc_int; ctors = [] };
    { tycon = tc_string; ctors = [] };
    dt_bool;
    dt_unit;
    dt_list;
  ]

let ctor_false = List.nth dt_bool.ctors 0
let ctor_true = List.nth dt_bool.ctors 1
let ctor_unit = List.nth dt_unit.ctors 0
let ctor_nil = List.nth dt_list.ctors 0
let ctor_cons = List.nth dt_list.ctors 1

(* Printing *)

let var_name n =
  if n < 26 then String.make 1 (Char.chr (Char.code 'a' + n)) else "t" ^ string_of_int n

let printer () =
  let vars = ref [] and gens = ref [] and count = ref 0 in
  let fresh_name () =
    let name = var_name !count in
    incr count;
    name
  in
  let name_var r =
    match List.assq_opt r !vars with
    | Some name -> name
    | None ->
        let name = fresh_name () in
        vars := (r, name) :: !vars;
        name
  in
  let name_gen i =
    match List.assoc_opt i !gens with
    | Some name -> name
    | None ->
        let name = fresh_name () in
        gens := (i, name) :: !gens;
        name
  in
  let rec go t =
    match repr t with
    | TVar r -> name_var r
    | TGen i -> name_gen i
    | TRigid (_, name) -> name
    | TCon (c, []) -> c.tc_name
    | TCon (c, args) -> "(" ^ String.concat " " (c.tc_name :: List.map go args) ^ ")"
    | TFun (params, result) ->
        (* Named separately because OCaml evaluates the right side of [@] first, and
           variables should get their letters in reading order. *)
        let params = List.map go params in
        let result = go result in
        "(" ^ String.concat " " (("->" :: params) @ [ result ]) ^ ")"
  in
  go

let to_string t = printer () t
let scheme_to_string { body; _ } = to_string body

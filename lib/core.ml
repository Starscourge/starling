(* The compiler's intermediate language. Compared with the typed tree, every variable is unique
   and resolved, sugar and bare constructors are gone, and pattern matching has become explicit
   tests.

   [Fatbar (a, b)] and [Fail] express backtracking in compiled matches: run [a], and if it
   reaches [Fail], continue with [b] instead. This keeps the code linear in the size of the
   patterns, because a clause body is never duplicated. *)

type span = Diagnostic.span
type var = { id : int; name : string }

let counter = ref 0

let fresh name =
  incr counter;
  { id = !counter; name }

type expr =
  | Const of Bytecode.value
  | Local of var
  | Global of int * string * span
  | SetGlobal of int * expr
  | Lambda of lambda
  | Call of expr * expr list * span
  | Prim of Bytecode.instr * expr list * span
  | MakeData of Bytecode.ctor_desc * expr list
  | Field of var * int
  | If of expr * expr * expr
  | Let of var * expr * expr
  | LetRec of (var * lambda) list * expr
  | Seq of expr * expr
  | Switch of var * (int * expr) list * int
      (** Dispatches on a constructor tag: [(tag, body)] cases, and the type's constructor
          count. A tag with no case means [Fail]. *)
  | Fatbar of expr * expr
  | Fail
  | MatchFailure of span

and lambda = { fn_name : string; params : var list; body : expr }

(* Variables are globally unique, so the free variables of a lambda are simply those it uses
   minus those it binds, and no scoping needs tracking. *)
let free_vars lambda =
  let used = Hashtbl.create 16 and bound = Hashtbl.create 16 in
  let use v = Hashtbl.replace used v.id v in
  let bind v = Hashtbl.replace bound v.id () in
  let rec go = function
    | Const _ | Global _ | Fail | MatchFailure _ -> ()
    | Local v | Field (v, _) -> use v
    | SetGlobal (_, e) -> go e
    | Lambda l -> go_lambda l
    | Call (f, args, _) ->
        go f;
        List.iter go args
    | Prim (_, args, _) | MakeData (_, args) -> List.iter go args
    | If (a, b, c) ->
        go a;
        go b;
        go c
    | Let (v, rhs, body) ->
        bind v;
        go rhs;
        go body
    | LetRec (bindings, body) ->
        List.iter
          (fun (v, l) ->
            bind v;
            go_lambda l)
          bindings;
        go body
    | Seq (a, b) | Fatbar (a, b) ->
        go a;
        go b
    | Switch (v, cases, _) ->
        use v;
        List.iter (fun (_, e) -> go e) cases
  and go_lambda l =
    List.iter bind l.params;
    go l.body
  in
  go_lambda lambda;
  Hashtbl.fold (fun id v acc -> if Hashtbl.mem bound id then acc else v :: acc) used []
  |> List.sort (fun a b -> compare a.id b.id)

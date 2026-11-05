(* The output of type inference: the surface language with every name resolved. Later phases
   no longer need to ask whether an identifier is a variable or a constructor, or which
   constructor a pattern refers to. *)

type span = Diagnostic.span

type pattern =
  | PWild
  | PVar of string
  | PInt of Bigint.t
  | PStr of string
  | PCtor of Types.ctor * pattern list

type expr = { desc : desc; span : span }

and desc =
  | Int of Bigint.t
  | Str of string
  | Bool of bool
  | Unit
  | Var of string
  | CtorApp of Types.ctor * expr list
      (** Saturated, so it can build the value directly. *)
  | CtorFn of Types.ctor
      (** A constructor used as a function value, as in [(map some xs)]. *)
  | ListLit of expr list
  | Lambda of string list * expr
  | App of expr * expr list
  | If of expr * expr * expr
  | Let of (string * expr) list * expr  (** Right-hand sides see only the outer scope. *)
  | LetRec of (string * expr) list * expr
  | Match of expr * (pattern * expr) list
  | Seq of expr list

type top = Define of string * expr | Expr of expr

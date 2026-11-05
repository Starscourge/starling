(* The surface language, after the parser has removed the purely syntactic sugar: [cond],
   [and], [or], [let*], [quote] and internal [define]s are already gone by this point. *)

type span = Diagnostic.span
type binder = { name : string; binder_span : span }

type ty_expr = { ty : ty_desc; ty_span : span }

and ty_desc =
  | TyName of string
  | TyApp of string * ty_expr list
  | TyFun of ty_expr list * ty_expr

type pattern = { pat : pat_desc; pat_span : span }

and pat_desc =
  | PWild
  | PIdent of string  (** A variable, or a nullary constructor. {!Infer} decides which. *)
  | PInt of Bigint.t
  | PStr of string
  | PCtor of string * pattern list

type expr = { desc : desc; span : span }

and desc =
  | Int of Bigint.t
  | Str of string
  | Bool of bool
  | Unit
  | Var of string
  | ListLit of expr list
  | Lambda of binder list * expr
  | App of expr * expr list
  | If of expr * expr * expr
  | Let of (binder * expr) list * expr
  | LetRec of (binder * expr) list * expr
  | Match of expr * (pattern * expr) list
  | Seq of expr list
  | The of ty_expr * expr

type ctor_decl = { ctor_name : string; ctor_span : span; fields : ty_expr list }

type type_decl = {
  type_name : string;
  type_span : span;
  params : binder list;
  ctors : ctor_decl list;
}

type top =
  | Define of binder * expr
  | Sig of binder * ty_expr
  | Type of type_decl
  | Expr of expr

(** The representation of types, shared by inference and match checking. *)

type tycon = { tc_name : string; tc_id : int; tc_arity : int }
(** A type constructor. Identity is [tc_id], not the name, so redefining a type in the
    REPL creates a genuinely different type. *)

type ty =
  | TVar of tvar ref  (** A unification variable. *)
  | TCon of tycon * ty list
  | TFun of ty list * ty  (** Functions have a fixed arity and are not curried. *)
  | TGen of int  (** The nth quantified variable of the enclosing {!scheme}. *)
  | TRigid of int * string
      (** A variable from a type signature, which must stay fully general. *)

and tvar =
  | Unbound of { id : int; level : int }
      (** [level] is the let-nesting depth the variable belongs to. See {!Infer}. *)
  | Link of ty  (** Already solved. Follow it with {!repr}. *)

type scheme = { quantified : int; body : ty }
(** A polymorphic type: for all of the [quantified] variables, [body]. *)

type ctor = {
  ctor_name : string;
  tag : int;  (** Position within the owning type, used for runtime dispatch. *)
  fields : ty list;  (** Field types over the owner's parameters, as [TGen]. *)
  owner : data_type;
}

and data_type = { tycon : tycon; mutable ctors : ctor list }

val mono : ty -> scheme

val repr : ty -> ty
(** Follows links, compressing the path as it goes. *)

val subst_gen : ty array -> ty -> ty
(** Replaces each [TGen i] with element [i]. *)

val next_id : unit -> int
(** A fresh identifier, unique across the process. *)

val new_tycon : string -> int -> tycon

(** {1 Built-in types} *)

val tc_int : tycon
val tc_string : tycon
val tc_bool : tycon
val tc_unit : tycon
val tc_list : tycon
val t_int : ty
val t_string : ty
val t_bool : ty
val t_unit : ty
val t_list : ty -> ty

val builtin_types : data_type list
(** Every built-in type. [int] and [string] have no constructors. *)

val ctor_true : ctor
val ctor_false : ctor
val ctor_unit : ctor
val ctor_nil : ctor
val ctor_cons : ctor

(** {1 Printing} *)

val printer : unit -> ty -> string
(** [printer ()] returns a function that names variables [a], [b], and so on, consistently
    across every type it prints. Create one per message. *)

val to_string : ty -> string
val scheme_to_string : scheme -> string

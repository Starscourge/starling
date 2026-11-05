(** Hindley-Milner type inference.

    This is Algorithm W with level-based generalization (Rémy 1992). Every unification
    variable records the [let] depth it belongs to. Generalizing a binding quantifies only
    the variables deeper than the current depth, which are exactly those that cannot be
    reached from the enclosing environment, and no environment scan is needed.

    The language has no mutable references, so every [let] can generalize, with no value
    restriction. *)

type env
(** Everything in scope: values, constructors and types. Immutable. *)

val initial_env : env

type result = {
  env : env;  (** The input environment extended with this unit's definitions. *)
  items : Typed.top list;  (** In source order. *)
  definitions : (string * Types.scheme) list;  (** In source order. *)
  last_type : Types.scheme option;
      (** The type of the unit's final form, if that form is an expression. *)
}

val check_program : env -> Syntax.top list -> result
(** @raise Diagnostic.Error on any type error. *)

val type_of_expr : env -> Syntax.expr -> Types.scheme
(** The most general type of a single expression, for the REPL's [:type] command.
    @raise Diagnostic.Error on any type error. *)

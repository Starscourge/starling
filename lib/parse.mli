(** Turns S-expressions into the surface AST, checking the shape of each special form and
    expanding the sugar that needs no type or scope information. *)

val program : Reader.sexp list -> Syntax.top list
(** @raise Diagnostic.Error on a malformed form. *)

val expr : Reader.sexp -> Syntax.expr
(** Parses a single expression. *)

val is_type_var : string -> bool
(** A type variable is one lowercase letter plus optional digits: [a], [b], [t1].
    Requiring this shape means a misspelled type name is reported rather than silently
    treated as a fresh variable. *)

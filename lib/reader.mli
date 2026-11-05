(** Reads tokens into S-expressions, keeping the source span of every node. *)

type sexp = { node : node; span : Diagnostic.span }
and node = Int of Bigint.t | Str of string | Sym of string | List of sexp list

val read : Diagnostic.source -> sexp list
(** Reads every form in the source. ['x] reads as [(quote x)].
    @raise Diagnostic.Error on malformed input, including unbalanced parentheses. *)

val is_complete : string -> bool
(** [false] when the text ends inside an open list or string, meaning more input could
    make it valid. Text with other errors counts as complete, so they get reported right
    away. *)

val to_string : sexp -> string
(** Prints a form back as text, for debugging and tests. *)

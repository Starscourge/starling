(** Checks a [match] for missing and unreachable cases, using the usefulness algorithm
    from Luc Maranget, "Warnings for pattern matching" (JFP 2007). *)

val check : Diagnostic.span -> (Typed.pattern * Diagnostic.span) list -> unit
(** [check span clauses] takes the [match] location and each clause's pattern and
    location.
    @raise Diagnostic.Error
      if a case is missing, with an example of a value that no clause handles, or if a
      clause can never match. *)

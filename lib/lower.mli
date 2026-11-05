(** Lowers the typed tree to {!Core}: resolves names to unique variables and global slots,
    and compiles pattern matching into explicit tests. *)

type globals
(** Which global slot each top-level name occupies. Persists across REPL inputs, so a new
    definition can shadow an old one while earlier code keeps the binding it was compiled
    with. *)

val no_globals : globals

val global_count : globals -> int
(** How many global slots the VM must provide. *)

val program : globals -> Typed.top list -> globals * Core.expr
(** Produces the body of the unit's entry point. Function definitions are initialized
    first, which is what makes functions visible throughout a file regardless of order.
    Everything else runs in source order. The result is the value of the unit's final
    expression, or [unit] if it ends in a definition. *)

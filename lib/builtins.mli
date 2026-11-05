(** The primitive operations: one table that gives each its name, type and instruction, so
    the type checker and the code generator cannot disagree. *)

type t = { name : string; arity : int; instr : Bytecode.instr; scheme : Types.scheme }

val all : t list
val find : string -> t option

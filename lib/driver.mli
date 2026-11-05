(** Runs the whole pipeline: text to tokens to forms to AST to types to core to bytecode
    to execution. A session keeps state across inputs, which is all the REPL needs. *)

type session

val create : ?out:(string -> unit) -> unit -> session
(** A new session with the standard library loaded. [out] receives program output. *)

type outcome = {
  value : Bytecode.value;  (** The value of the final expression, or [unit]. *)
  value_type : Types.scheme option;  (** Set if the input ended in an expression. *)
  definitions : (string * Types.scheme) list;
}

val eval : session -> Diagnostic.source -> outcome
(** Compiles and runs one unit of source against the session.
    @raise Diagnostic.Error on a compile error, which leaves the session unchanged.
    @raise Vm.Runtime_error if the program fails. *)

val check : session -> Diagnostic.source -> (string * Types.scheme) list
(** Type-checks without running, and returns the type of each definition.
    @raise Diagnostic.Error on a compile error. *)

val type_of : session -> Diagnostic.source -> Types.scheme
(** The type of a single expression.
    @raise Diagnostic.Error on a compile error, or if the source isn't one expression. *)

val disassemble : session -> Diagnostic.source -> string
(** The bytecode for one unit of source, without running it.
    @raise Diagnostic.Error on a compile error. *)

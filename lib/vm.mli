(** The bytecode interpreter.

    A single loop dispatches on instructions over one explicit value stack. Starling calls
    do not use the native OCaml stack at all, so recursion depth is limited only by the
    VM's own limit and hitting that limit is an ordinary runtime error. Tail calls reuse
    the current frame, so tail-recursive loops run in constant stack space. *)

type t
(** Global variables and stack storage. Persists between runs, which is what lets a REPL
    session build up state. *)

type frame = { fn_name : string; location : Diagnostic.span }

type error = {
  message : string;
  trace : frame list;
      (** Active calls, innermost first. Very deep stacks keep only the frames at each
          end. *)
  hidden_calls : int;  (** How many frames were left out of the middle of [trace]. *)
}

exception Runtime_error of error

val create : ?out:(string -> unit) -> unit -> t
(** [out] receives everything the program prints. It defaults to standard output. *)

val run : t -> n_globals:int -> Bytecode.code -> Bytecode.value
(** Runs a zero-argument entry point and returns its result.
    @raise Runtime_error if the program fails. *)

val render_error : error -> string
(** Formats a runtime error and its stack trace for the user. *)

(** Source locations and compiler errors.

    Every phase reports problems the same way: raise {!Error} with a span, and let the
    driver render it with the offending source line and a caret underline. *)

type source = { name : string; text : string }

type span = { source : source; start : int; stop : int }
(** Byte offsets into [source.text]. [stop] is exclusive. *)

type t = { message : string; span : span; notes : string list }

exception Error of t

val error : ?notes:string list -> span -> string -> 'a
(** Raises {!Error}. *)

val errorf : ?notes:string list -> span -> ('a, unit, string, 'b) format4 -> 'a
(** Like {!error}, with a format string. *)

val merge : span -> span -> span
(** The smallest span covering both arguments, which must share a source. *)

val none : span
(** A placeholder for compiler-generated code with no source location. *)

val line_col : source -> int -> int * int
(** [line_col source offset] is the 1-based line and column of [offset]. *)

val location : span -> string
(** Formats as [name:line:col]. *)

val render : ?color:bool -> t -> string
(** The full multi-line report. *)

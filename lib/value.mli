(** Operations on runtime values.

    Integers have two representations. [Int] holds anything that fits in a native [int],
    and results that overflow are promoted to [Big]. The two never overlap: a [Big] result
    that fits goes back to [Int], so one number always has one representation. *)

open Bytecode

exception Error of string
(** A runtime failure. The VM adds the location and stack trace. *)

val of_bigint : Bigint.t -> value
(** The canonical representation of an integer. *)

val to_bigint : value -> Bigint.t
(** The value must be an integer. *)

(** {1 Integer arithmetic} *)

val add : value -> value -> value
val sub : value -> value -> value
val mul : value -> value -> value

val quot : value -> value -> value
(** @raise Error on division by zero. *)

val rem : value -> value -> value
(** @raise Error on division by zero. *)

val neg : value -> value
val compare_ints : value -> value -> int

(** {1 Equality and printing} *)

val equal : value -> value -> bool
(** Structural equality. Uses an explicit stack, so long lists can't overflow the native
    one.
    @raise Error if asked to compare functions. *)

val repr : value -> string
(** The form the REPL shows: strings are quoted and escaped. *)

val display : value -> string
(** The form [print] and [show] use: like {!repr}, but a top-level string is left as is.
*)

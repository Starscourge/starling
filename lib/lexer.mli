(** Hand-written scanner from source text to tokens. *)

type token =
  | LParen
  | RParen
  | Quote
  | Int of Bigint.t
  | Str of string
  | Sym of string
  | Eof

type t

exception Incomplete of Diagnostic.t
(** The input ended inside a construct that more text could finish, such as a string. The
    REPL uses this to prompt for another line rather than report an error. *)

val create : Diagnostic.source -> t

val next : t -> token * Diagnostic.span
(** Skips whitespace and comments, then returns the next token. Returns [Eof] forever once
    the input is exhausted.
    @raise Diagnostic.Error on malformed input.
    @raise Incomplete on an unterminated string. *)

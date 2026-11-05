(** Arbitrary-precision signed integers.

    Values are immutable. The representation is sign and magnitude, with the magnitude
    stored as a little-endian array of base 10{^ 9} limbs. A decimal base makes printing
    linear, and a limb product still fits comfortably in OCaml's 63-bit [int].

    Multiplication uses Karatsuba above a small threshold and division uses Knuth's
    Algorithm D. *)

type t

val zero : t
val one : t

(** {1 Conversions} *)

val of_int : int -> t

val to_int : t -> int option
(** [to_int x] is [Some n] when [x] fits in a native [int]. *)

val of_string : string -> t
(** [of_string s] parses an optional sign followed by one or more decimal digits.
    @raise Invalid_argument on any other input. *)

val of_string_opt : string -> t option
val to_string : t -> string

(** {1 Comparison} *)

val compare : t -> t -> int
val equal : t -> t -> bool

val sign : t -> int
(** [sign x] is [-1], [0] or [1]. *)

val is_zero : t -> bool

(** {1 Arithmetic} *)

val neg : t -> t
val abs : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t

val quo_rem : t -> t -> t * t
(** [quo_rem a b] is truncating division: the quotient rounds toward zero and the
    remainder takes the sign of [a], so [a = quo * b + rem].
    @raise Division_by_zero if [b] is zero. *)

val quo : t -> t -> t
val rem : t -> t -> t

val pow : t -> int -> t
(** [pow x n] raises [x] to the power [n] by repeated squaring.
    @raise Invalid_argument if [n] is negative. *)

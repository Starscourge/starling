let base = 1_000_000_000
let base_digits = 9

(* Below this many limbs, schoolbook multiplication beats Karatsuba's overhead. *)
let karatsuba_threshold = 32

(* Invariants: [mag] is little-endian with no high zero limb, and [sign = 0] exactly when
   [mag] is empty. Every function below preserves them, so structural comparison of limbs
   is meaningful. *)
type t = { sign : int; mag : int array }

let zero = { sign = 0; mag = [||] }

(* Magnitude helpers. All of them treat their inputs as read-only. *)

let trim a =
  let n = ref (Array.length a) in
  while !n > 0 && a.(!n - 1) = 0 do
    decr n
  done;
  if !n = Array.length a then a else Array.sub a 0 !n

let make sign mag =
  let mag = trim mag in
  if Array.length mag = 0 then zero else { sign; mag }

let mag_compare a b =
  let la = Array.length a and lb = Array.length b in
  if la <> lb then compare la lb
  else
    let rec go i =
      if i < 0 then 0 else if a.(i) <> b.(i) then compare a.(i) b.(i) else go (i - 1)
    in
    go (la - 1)

let mag_add a b =
  let a, b = if Array.length a >= Array.length b then (a, b) else (b, a) in
  let la = Array.length a and lb = Array.length b in
  let r = Array.make (la + 1) 0 in
  let carry = ref 0 in
  for i = 0 to la - 1 do
    let s = a.(i) + (if i < lb then b.(i) else 0) + !carry in
    if s >= base then (
      r.(i) <- s - base;
      carry := 1)
    else (
      r.(i) <- s;
      carry := 0)
  done;
  r.(la) <- !carry;
  trim r

(* Requires [a >= b]. *)
let mag_sub a b =
  let la = Array.length a and lb = Array.length b in
  let r = Array.make la 0 in
  let borrow = ref 0 in
  for i = 0 to la - 1 do
    let d = a.(i) - (if i < lb then b.(i) else 0) - !borrow in
    if d < 0 then (
      r.(i) <- d + base;
      borrow := 1)
    else (
      r.(i) <- d;
      borrow := 0)
  done;
  trim r

let mag_mul_school a b =
  let la = Array.length a and lb = Array.length b in
  let r = Array.make (la + lb) 0 in
  for i = 0 to la - 1 do
    let ai = a.(i) in
    if ai <> 0 then (
      let carry = ref 0 in
      for j = 0 to lb - 1 do
        (* At most (base-1) + (base-1)^2 + (base-1), which is below 10^18 and so below
           max_int. *)
        let cur = r.(i + j) + (ai * b.(j)) + !carry in
        r.(i + j) <- cur mod base;
        carry := cur / base
      done;
      r.(i + lb) <- !carry)
  done;
  trim r

(* Adds [a] into [r] starting at limb [offset]. [r] must be long enough to absorb the carry. *)
let add_into r offset a =
  let la = Array.length a in
  let carry = ref 0 in
  let i = ref 0 in
  while !i < la || !carry <> 0 do
    let s = r.(offset + !i) + (if !i < la then a.(!i) else 0) + !carry in
    if s >= base then (
      r.(offset + !i) <- s - base;
      carry := 1)
    else (
      r.(offset + !i) <- s;
      carry := 0);
    incr i
  done

let low a k = trim (Array.sub a 0 (min k (Array.length a)))
let high a k = if Array.length a <= k then [||] else Array.sub a k (Array.length a - k)

let rec mag_mul a b =
  let la = Array.length a and lb = Array.length b in
  if la = 0 || lb = 0 then [||]
  else if min la lb < karatsuba_threshold then mag_mul_school a b
  else
    (* With a = a1*B^k + a0 and b = b1*B^k + b0, the middle term comes from one extra
       multiplication instead of two:
       a1*b0 + a0*b1 = (a0+a1)(b0+b1) - a1*b1 - a0*b0. *)
    let k = max la lb / 2 in
    let a0 = low a k and a1 = high a k in
    let b0 = low b k and b1 = high b k in
    let z0 = mag_mul a0 b0 in
    let z2 = mag_mul a1 b1 in
    let z1 = mag_sub (mag_sub (mag_mul (mag_add a0 a1) (mag_add b0 b1)) z2) z0 in
    let r = Array.make (la + lb + 1) 0 in
    add_into r 0 z0;
    add_into r k z1;
    add_into r (2 * k) z2;
    trim r

(* Divides by a single limb. *)
let mag_divmod_small a d =
  let n = Array.length a in
  let q = Array.make n 0 in
  let rem = ref 0 in
  for i = n - 1 downto 0 do
    let cur = (!rem * base) + a.(i) in
    q.(i) <- cur / d;
    rem := cur mod d
  done;
  (trim q, !rem)

(* Multiplies by a single limb into an array exactly [len] limbs long. *)
let mag_scale a d len =
  let r = Array.make len 0 in
  let carry = ref 0 in
  for i = 0 to len - 1 do
    let limb = if i < Array.length a then a.(i) else 0 in
    let cur = (limb * d) + !carry in
    r.(i) <- cur mod base;
    carry := cur / base
  done;
  assert (!carry = 0);
  r

(* Knuth, The Art of Computer Programming vol. 2, section 4.3.1, Algorithm D.
   Requires [u >= v] and at least two limbs in [v]. *)
let mag_divmod_knuth u v =
  let n = Array.length v in
  let m = Array.length u - n in
  (* Normalize so the divisor's top limb is at least base/2, which keeps the estimate of
     each quotient digit within 2 of the true value. *)
  let d = base / (v.(n - 1) + 1) in
  let u = mag_scale u d (Array.length u + 1) in
  let v = mag_scale v d n in
  let vtop = v.(n - 1) and vnext = v.(n - 2) in
  let q = Array.make (m + 1) 0 in
  for j = m downto 0 do
    let num = (u.(j + n) * base) + u.(j + n - 1) in
    let rec refine qhat rhat =
      if rhat < base && (qhat >= base || qhat * vnext > (rhat * base) + u.(j + n - 2))
      then refine (qhat - 1) (rhat + vtop)
      else qhat
    in
    let qhat = ref (refine (num / vtop) (num mod vtop)) in
    (* Multiply and subtract qhat * v from the current window of u. *)
    let carry = ref 0 and borrow = ref 0 in
    for i = 0 to n - 1 do
      let p = (!qhat * v.(i)) + !carry in
      carry := p / base;
      let t = u.(i + j) - (p mod base) - !borrow in
      if t < 0 then (
        u.(i + j) <- t + base;
        borrow := 1)
      else (
        u.(i + j) <- t;
        borrow := 0)
    done;
    let t = u.(j + n) - !carry - !borrow in
    if t < 0 then (
      (* The estimate was one too high. This is rare: add the divisor back. *)
      decr qhat;
      let c = ref 0 in
      for i = 0 to n - 1 do
        let s = u.(i + j) + v.(i) + !c in
        if s >= base then (
          u.(i + j) <- s - base;
          c := 1)
        else (
          u.(i + j) <- s;
          c := 0)
      done;
      u.(j + n) <- t + !c)
    else u.(j + n) <- t;
    q.(j) <- !qhat
  done;
  let remainder, _ = mag_divmod_small (trim (Array.sub u 0 n)) d in
  (trim q, remainder)

let mag_divmod a b =
  if mag_compare a b < 0 then ([||], a)
  else if Array.length b = 1 then
    let q, r = mag_divmod_small a b.(0) in
    (q, if r = 0 then [||] else [| r |])
  else mag_divmod_knuth a b

(* Conversions *)

let of_int n =
  if n = 0 then zero
  else
    let sign = if n < 0 then -1 else 1 in
    (* Work on the negative value, because [min_int] has no positive counterpart. *)
    let rec limbs acc m =
      if m = 0 then List.rev acc else limbs (-(m mod base) :: acc) (m / base)
    in
    { sign; mag = Array.of_list (limbs [] (if n > 0 then -n else n)) }

let one = of_int 1

let to_int { sign; mag } =
  match Array.length mag with
  | 0 -> Some 0
  | 1 -> Some (sign * mag.(0))
  | 2 -> Some (sign * ((mag.(1) * base) + mag.(0)))
  | 3 when mag.(2) <= 4 ->
      (* 4 * 10^18 fits but the sum may not, so compare before adding. *)
      let high = mag.(2) * base * base in
      let low = (mag.(1) * base) + mag.(0) in
      if sign > 0 then if low <= max_int - high then Some (high + low) else None
      else if -low >= min_int + high then Some (-high - low)
      else None
  | _ -> None

let of_string_opt s =
  let len = String.length s in
  let negative = len > 0 && s.[0] = '-' in
  let start = if len > 0 && (s.[0] = '-' || s.[0] = '+') then 1 else 0 in
  let is_digit c = c >= '0' && c <= '9' in
  let rec all_digits i = i >= len || (is_digit s.[i] && all_digits (i + 1)) in
  if start >= len || not (all_digits start) then None
  else
    let digits = len - start in
    let limbs = (digits + base_digits - 1) / base_digits in
    let mag = Array.make limbs 0 in
    for i = 0 to limbs - 1 do
      let stop = len - (i * base_digits) in
      let first = max start (stop - base_digits) in
      let limb = ref 0 in
      for k = first to stop - 1 do
        limb := (!limb * 10) + (Char.code s.[k] - Char.code '0')
      done;
      mag.(i) <- !limb
    done;
    Some (make (if negative then -1 else 1) mag)

let of_string s =
  match of_string_opt s with Some x -> x | None -> invalid_arg ("Bigint.of_string: " ^ s)

let to_string { sign; mag } =
  let n = Array.length mag in
  if n = 0 then "0"
  else
    let buf = Buffer.create ((n * base_digits) + 1) in
    if sign < 0 then Buffer.add_char buf '-';
    Buffer.add_string buf (string_of_int mag.(n - 1));
    for i = n - 2 downto 0 do
      Buffer.add_string buf (Printf.sprintf "%09d" mag.(i))
    done;
    Buffer.contents buf

(* Comparison *)

let sign x = x.sign
let is_zero x = x.sign = 0

let compare a b =
  if a.sign <> b.sign then Stdlib.compare a.sign b.sign
  else if a.sign >= 0 then mag_compare a.mag b.mag
  else mag_compare b.mag a.mag

let equal a b = compare a b = 0

(* Arithmetic *)

let neg x = { x with sign = -x.sign }
let abs x = { x with sign = Stdlib.abs x.sign }

let add a b =
  if a.sign = 0 then b
  else if b.sign = 0 then a
  else if a.sign = b.sign then { sign = a.sign; mag = mag_add a.mag b.mag }
  else
    let c = mag_compare a.mag b.mag in
    if c = 0 then zero
    else if c > 0 then make a.sign (mag_sub a.mag b.mag)
    else make b.sign (mag_sub b.mag a.mag)

let sub a b = add a (neg b)

let mul a b =
  if a.sign = 0 || b.sign = 0 then zero else make (a.sign * b.sign) (mag_mul a.mag b.mag)

let quo_rem a b =
  if b.sign = 0 then raise Division_by_zero
  else
    let q, r = mag_divmod a.mag b.mag in
    (make (a.sign * b.sign) q, make a.sign r)

let quo a b = fst (quo_rem a b)
let rem a b = snd (quo_rem a b)

let pow x n =
  if n < 0 then invalid_arg "Bigint.pow: negative exponent"
  else
    let rec go acc x n =
      if n = 0 then acc
      else
        let acc = if n land 1 = 1 then mul acc x else acc in
        if n = 1 then acc else go acc (mul x x) (n lsr 1)
    in
    go one x n

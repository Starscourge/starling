(* Bigint is checked differentially: every operation must agree with Zarith, an established
   GMP-backed library, across operand sizes that exercise each algorithm. *)

open Starling

let big = Alcotest.testable (Fmt.of_to_string Bigint.to_string) Bigint.equal
let b = Bigint.of_string

let test_to_string () =
  let roundtrip s = Alcotest.(check string) s s (Bigint.to_string (b s)) in
  List.iter roundtrip
    [
      "0";
      "7";
      "-7";
      "999999999";
      "1000000000";
      "-1000000000";
      "1000000000000000000000000000";
      "123456789012345678901234567890";
    ]

let test_normalizes_input () =
  Alcotest.check big "leading zeros" (b "42") (b "0000042");
  Alcotest.check big "negative zero" Bigint.zero (b "-0");
  Alcotest.check big "explicit plus" (b "5") (b "+5")

let test_rejects_bad_strings () =
  let rejected s = Alcotest.(check bool) s true (Bigint.of_string_opt s = None) in
  List.iter rejected [ ""; "-"; "+"; "12a4"; "1 2"; "--1"; "0x10" ]

let test_native_int_limits () =
  let check n =
    let x = Bigint.of_int n in
    Alcotest.(check string) "to_string" (string_of_int n) (Bigint.to_string x);
    Alcotest.(check (option int)) "to_int" (Some n) (Bigint.to_int x)
  in
  List.iter check [ 0; 1; -1; max_int; min_int; max_int - 1; min_int + 1 ]

let test_to_int_overflow () =
  let one_past n = Bigint.add (Bigint.of_int n) (Bigint.of_int (compare n 0)) in
  Alcotest.(check (option int)) "max_int + 1" None (Bigint.to_int (one_past max_int));
  Alcotest.(check (option int)) "min_int - 1" None (Bigint.to_int (one_past min_int))

let test_division_by_zero () =
  Alcotest.check_raises "quo" Division_by_zero (fun () ->
      ignore (Bigint.quo Bigint.one Bigint.zero));
  Alcotest.check_raises "rem" Division_by_zero (fun () ->
      ignore (Bigint.rem Bigint.one Bigint.zero))

let test_truncating_division () =
  let check a d q r =
    let got_q, got_r = Bigint.quo_rem (Bigint.of_int a) (Bigint.of_int d) in
    Alcotest.check big "quo" (Bigint.of_int q) got_q;
    Alcotest.check big "rem" (Bigint.of_int r) got_r
  in
  check 7 2 3 1;
  check (-7) 2 (-3) (-1);
  check 7 (-2) (-3) 1;
  check (-7) (-2) 3 (-1)

let test_pow () =
  Alcotest.check big "2^100"
    (b "1267650600228229401496703205376")
    (Bigint.pow (Bigint.of_int 2) 100);
  Alcotest.check big "x^0" Bigint.one (Bigint.pow (Bigint.of_int 12345) 0);
  Alcotest.check big "0^0" Bigint.one (Bigint.pow Bigint.zero 0);
  Alcotest.check_raises "negative exponent"
    (Invalid_argument "Bigint.pow: negative exponent") (fun () ->
      ignore (Bigint.pow Bigint.one (-1)))

(* Generators. Mostly-0 and mostly-9 digit strings produce the carries, borrows and quotient
   overestimates that uniformly random digits almost never reach, including the rare
   add-back branch of Algorithm D. *)

let gen_digits =
  let open QCheck2.Gen in
  let digit =
    oneof
      [
        char_range '0' '9';
        oneof_weighted [ (9, pure '9'); (1, char_range '0' '9') ];
        oneof_weighted [ (9, pure '0'); (1, char_range '0' '9') ];
      ]
  in
  let* len =
    oneof_weighted
      [
        (5, int_range 1 20);
        (4, int_range 21 300);
        (3, int_range 301 1500);
        (1, int_range 1501 4000);
      ]
  in
  let* d = digit in
  let* uniform = bool in
  if uniform then map (fun c -> String.make len c) (pure d)
  else string_size ~gen:digit (pure len)

let gen_decimal =
  let open QCheck2.Gen in
  let* digits = gen_digits in
  let* negative = bool in
  pure (if negative then "-" ^ digits else digits)

let gen_pair = QCheck2.Gen.pair gen_decimal gen_decimal
let print_pair (x, y) = x ^ " , " ^ y

(* A dividend built as q*d + r, so quotients of every size come up, and small divisors do
   too. *)
let gen_division =
  let open QCheck2.Gen in
  let* q = gen_decimal and* d = gen_decimal and* r = gen_decimal in
  let dz = Z.of_string d in
  let dz = if Z.equal dz Z.zero then Z.one else dz in
  let rz = Z.rem (Z.of_string r) dz in
  pure (Z.to_string (Z.add (Z.mul (Z.of_string q) dz) rz), Z.to_string dz)

let agrees name gen print op op_z =
  QCheck2.Test.make ~name ~count:400 ~print gen (fun (x, y) ->
      Bigint.to_string (op (b x) (b y))
      = Z.to_string (op_z (Z.of_string x) (Z.of_string y)))

let properties =
  [
    QCheck2.Test.make ~name:"to_string agrees with Zarith" ~count:400 ~print:Fun.id
      gen_decimal (fun s -> Bigint.to_string (b s) = Z.to_string (Z.of_string s));
    agrees "add agrees with Zarith" gen_pair print_pair Bigint.add Z.add;
    agrees "sub agrees with Zarith" gen_pair print_pair Bigint.sub Z.sub;
    agrees "mul agrees with Zarith" gen_pair print_pair Bigint.mul Z.mul;
    agrees "quo agrees with Zarith" gen_division print_pair Bigint.quo Z.div;
    agrees "rem agrees with Zarith" gen_division print_pair Bigint.rem Z.rem;
    QCheck2.Test.make ~name:"compare agrees with Zarith" ~count:400 ~print:print_pair
      gen_pair (fun (x, y) ->
        Int.compare (Bigint.compare (b x) (b y)) 0
        = Int.compare (Z.compare (Z.of_string x) (Z.of_string y)) 0);
    QCheck2.Test.make ~name:"division identity a = q*d + r, |r| < |d|" ~count:400
      ~print:print_pair gen_division (fun (x, y) ->
        let a = b x and d = b y in
        let q, r = Bigint.quo_rem a d in
        Bigint.equal a (Bigint.add (Bigint.mul q d) r)
        && Bigint.compare (Bigint.abs r) (Bigint.abs d) < 0);
    QCheck2.Test.make ~name:"to_int agrees with Zarith" ~count:400 ~print:Fun.id
      gen_decimal (fun s ->
        let z = Z.of_string s in
        let expected = if Z.fits_int z then Some (Z.to_int z) else None in
        Bigint.to_int (b s) = expected);
  ]

let suite =
  [
    Alcotest.test_case "to_string" `Quick test_to_string;
    Alcotest.test_case "normalizes input" `Quick test_normalizes_input;
    Alcotest.test_case "rejects bad strings" `Quick test_rejects_bad_strings;
    Alcotest.test_case "native int limits" `Quick test_native_int_limits;
    Alcotest.test_case "to_int overflow" `Quick test_to_int_overflow;
    Alcotest.test_case "division by zero" `Quick test_division_by_zero;
    Alcotest.test_case "truncating division" `Quick test_truncating_division;
    Alcotest.test_case "pow" `Quick test_pow;
  ]
  @ List.map QCheck_alcotest.to_alcotest properties

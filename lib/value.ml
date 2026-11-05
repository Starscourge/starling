open Bytecode

exception Error of string

let of_bigint n = match Bigint.to_int n with Some i -> Int i | None -> Big n

let to_bigint = function
  | Int i -> Bigint.of_int i
  | Big n -> n
  | _ -> invalid_arg "Value.to_bigint: not an integer"

let via_bigint op a b = of_bigint (op (to_bigint a) (to_bigint b))

let add a b =
  match (a, b) with
  | Int x, Int y ->
      let sum = x + y in
      (* Overflow happened exactly when both operands' signs differ from the result's. *)
      if x lxor sum land (y lxor sum) < 0 then via_bigint Bigint.add a b else Int sum
  | _ -> via_bigint Bigint.add a b

let sub a b =
  match (a, b) with
  | Int x, Int y ->
      let diff = x - y in
      if x lxor y land (x lxor diff) < 0 then via_bigint Bigint.sub a b else Int diff
  | _ -> via_bigint Bigint.sub a b

(* Operands under 2^31 in magnitude can't overflow a 63-bit product. *)
let small_limit = 1 lsl 31

let mul a b =
  match (a, b) with
  | Int x, Int y
    when x > -small_limit && x < small_limit && y > -small_limit && y < small_limit ->
      Int (x * y)
  | _ -> via_bigint Bigint.mul a b

let check_divisor = function Int 0 -> raise (Error "division by zero") | _ -> ()

let quot a b =
  check_divisor b;
  match (a, b) with
  (* min_int / -1 overflows, so -1 takes the slow path. *)
  | Int x, Int y when y <> -1 -> Int (x / y)
  | _ -> via_bigint Bigint.quo a b

let rem a b =
  check_divisor b;
  match (a, b) with
  | Int x, Int y when y <> -1 -> Int (x mod y)
  | _ -> via_bigint Bigint.rem a b

let neg = function
  | Int x when x <> min_int -> Int (-x)
  | v -> of_bigint (Bigint.neg (to_bigint v))

let compare_ints a b =
  match (a, b) with
  | Int x, Int y -> Int.compare x y
  | _ -> Bigint.compare (to_bigint a) (to_bigint b)

let equal a b =
  let rec go = function
    | [] -> true
    | pair :: rest -> (
        match pair with
        | Int x, Int y -> x = y && go rest
        | Big x, Big y -> Bigint.equal x y && go rest
        | Str x, Str y -> String.equal x y && go rest
        | True, True | False, False | Unit, Unit -> go rest
        | Data (c1, fields1), Data (c2, fields2) ->
            c1.tag = c2.tag
            &&
            let stack = ref rest in
            for i = Array.length fields1 - 1 downto 0 do
              stack := (fields1.(i), fields2.(i)) :: !stack
            done;
            go !stack
        | Closure _, _ | _, Closure _ ->
            raise (Error "functions cannot be compared with '='")
        | _ -> false)
  in
  go [ (a, b) ]

(* The escapes the lexer understands, so printed data reads back as the same value. Other bytes
   pass through, which keeps UTF-8 text readable. *)
let write_escaped buf s =
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\r' -> Buffer.add_string buf "\\r"
      | c -> Buffer.add_char buf c)
    s

let rec write buf v =
  match v with
  | Unit -> Buffer.add_string buf "unit"
  | True -> Buffer.add_string buf "true"
  | False -> Buffer.add_string buf "false"
  | Undefined -> Buffer.add_string buf "<undefined>"
  | Int n -> Buffer.add_string buf (string_of_int n)
  | Big n -> Buffer.add_string buf (Bigint.to_string n)
  | Str s ->
      Buffer.add_char buf '"';
      write_escaped buf s;
      Buffer.add_char buf '"'
  | Closure c ->
      Buffer.add_string buf "<function ";
      Buffer.add_string buf c.code.name;
      Buffer.add_char buf '>'
  | Data ({ is_list = true; _ }, _) ->
      Buffer.add_char buf '(';
      write_list_items buf v true;
      Buffer.add_char buf ')'
  | Data (c, [||]) -> Buffer.add_string buf c.name
  | Data (c, fields) ->
      Buffer.add_char buf '(';
      Buffer.add_string buf c.name;
      Array.iter
        (fun field ->
          Buffer.add_char buf ' ';
          write buf field)
        fields;
      Buffer.add_char buf ')'

(* A loop along the spine, so a long list doesn't use native stack proportional to its
   length. *)
and write_list_items buf v first =
  match v with
  | Data (_, [| head; tail |]) ->
      if not first then Buffer.add_char buf ' ';
      write buf head;
      write_list_items buf tail false
  | _ -> ()

let repr v =
  let buf = Buffer.create 32 in
  write buf v;
  Buffer.contents buf

let display = function Str s -> s | v -> repr v

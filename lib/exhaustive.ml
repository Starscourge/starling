(* The algorithm works on a matrix whose rows are clauses and whose columns are the values
   being matched. The central question is whether a new row is "useful": does some value
   match it and no earlier row? A clause is unreachable if it is not useful against the rows
   above it, and a match is exhaustive if an all-wildcard row is not useful against all of
   them. *)

type literal = LInt of Bigint.t | LStr of string
type pat = Any | Con of Types.ctor * pat list | Lit of literal

let rec simplify : Typed.pattern -> pat = function
  | PWild | PVar _ -> Any
  | PInt n -> Lit (LInt n)
  | PStr s -> Lit (LStr s)
  | PCtor (c, args) -> Con (c, List.map simplify args)

let wildcards n = List.init n (fun _ -> Any)
let arity (c : Types.ctor) = List.length c.fields

let same_literal a b =
  match (a, b) with
  | LInt x, LInt y -> Bigint.equal x y
  | LStr x, LStr y -> String.equal x y
  | _ -> false

(* The rows that can still match once the first column is known to be constructor [c]. The
   column is replaced by one column per field of [c]. *)
let specialize_ctor (c : Types.ctor) rows =
  List.filter_map
    (function
      | Con (c', args) :: rest -> if c'.tag = c.tag then Some (args @ rest) else None
      | Any :: rest -> Some (wildcards (arity c) @ rest)
      | Lit _ :: _ | [] -> None)
    rows

let specialize_literal lit rows =
  List.filter_map
    (function
      | Lit l :: rest -> if same_literal l lit then Some rest else None
      | Any :: rest -> Some rest
      | Con _ :: _ | [] -> None)
    rows

(* The rows that can still match when the first column is none of the values listed. *)
let default rows = List.filter_map (function Any :: rest -> Some rest | _ -> None) rows

(* The constructors appearing at the head of the first column, without duplicates. *)
let head_ctors rows =
  let add seen = function
    | Con ((c : Types.ctor), _) :: _
      when not (List.exists (fun (s : Types.ctor) -> s.tag = c.tag) seen) ->
        c :: seen
    | _ -> seen
  in
  List.fold_left add [] rows

(* Whether the head column names every constructor of its type. Literal columns never do,
   since there are infinitely many ints and strings. *)
let is_complete (seen : Types.ctor list) =
  match seen with [] -> false | c :: _ -> List.length seen = List.length c.owner.ctors

let rec useful rows vector =
  match vector with
  | [] -> ( match rows with [] -> true | _ :: _ -> false)
  | Con (c, args) :: rest -> useful (specialize_ctor c rows) (args @ rest)
  | Lit lit :: rest -> useful (specialize_literal lit rows) rest
  | Any :: rest ->
      let seen = head_ctors rows in
      if is_complete seen then
        List.exists
          (fun c -> useful (specialize_ctor c rows) (wildcards (arity c) @ rest))
          seen
      else useful (default rows) rest

let rec split n list =
  if n = 0 then ([], list)
  else
    match list with
    | x :: rest ->
        let first, second = split (n - 1) rest in
        (x :: first, second)
    | [] -> assert false

(* Like [useful] with an all-wildcard vector of width [n], but returns a concrete example of
   an unmatched value rather than just true or false. *)
let rec missing rows n =
  if n = 0 then match rows with [] -> Some [] | _ :: _ -> None
  else
    let seen = head_ctors rows in
    if is_complete seen then
      List.find_map
        (fun c ->
          let a = arity c in
          match missing (specialize_ctor c rows) (a + n - 1) with
          | None -> None
          | Some example ->
              let args, rest = split a example in
              Some (Con (c, args) :: rest))
        seen
    else
      match missing (default rows) (n - 1) with
      | None -> None
      | Some rest -> (
          match seen with
          | [] -> Some (Any :: rest)
          | c :: _ ->
              (* Some constructors were handled, so name one that was not. *)
              let not_seen (other : Types.ctor) =
                not (List.exists (fun (s : Types.ctor) -> s.tag = other.tag) seen)
              in
              let absent = List.find not_seen c.owner.ctors in
              Some (Con (absent, wildcards (arity absent)) :: rest))

let rec pat_to_string = function
  | Any -> "_"
  | Lit (LInt n) -> Bigint.to_string n
  | Lit (LStr s) -> "\"" ^ String.escaped s ^ "\""
  | Con (c, []) -> c.ctor_name
  | Con (c, args) ->
      "(" ^ String.concat " " (c.ctor_name :: List.map pat_to_string args) ^ ")"

let check span clauses =
  let rows_so_far =
    List.fold_left
      (fun rows (pattern, clause_span) ->
        let row = [ simplify pattern ] in
        if not (useful rows row) then
          Diagnostic.error clause_span
            "this clause can never match, because earlier clauses already cover it";
        rows @ [ row ])
      [] clauses
  in
  match missing rows_so_far 1 with
  | None -> ()
  | Some example ->
      let example = String.concat " " (List.map pat_to_string example) in
      Diagnostic.error span
        ~notes:[ "a value not handled: " ^ example ]
        "this match does not cover every case"

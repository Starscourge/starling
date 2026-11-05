open Types

type t = { name : string; arity : int; instr : Bytecode.instr; scheme : scheme }

let prim name instr params result =
  { name; arity = List.length params; instr; scheme = mono (TFun (params, result)) }

(* A primitive that works at any type, for all a. *)
let poly name instr params result =
  {
    name;
    arity = List.length params;
    instr;
    scheme = { quantified = 1; body = TFun (params, result) };
  }

let arith name instr = prim name instr [ t_int; t_int ] t_int
let compare_ints name instr = prim name instr [ t_int; t_int ] t_bool
let any = TGen 0

let all : t list =
  [
    arith "+" Add;
    arith "-" Sub;
    arith "*" Mul;
    arith "quot" Quot;
    arith "rem" Rem;
    prim "neg" Neg [ t_int ] t_int;
    compare_ints "<" Lt;
    compare_ints "<=" Le;
    compare_ints ">" Gt;
    compare_ints ">=" Ge;
    poly "=" Eq [ any; any ] t_bool;
    poly "!=" Ne [ any; any ] t_bool;
    prim "not" Not [ t_bool ] t_bool;
    prim "string-append" StrAppend [ t_string; t_string ] t_string;
    prim "string-length" StrLength [ t_string ] t_int;
    prim "substring" Substring [ t_string; t_int; t_int ] t_string;
    poly "show" Show [ any ] t_string;
    poly "print" Print [ any ] t_unit;
    poly "error" Raise [ t_string ] any;
  ]

let find name = List.find_opt (fun b -> b.name = name) all

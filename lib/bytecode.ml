type ctor_desc = { name : string; tag : int; is_list : bool }

type value =
  | Unit
  | True
  | False
  | Undefined
  | Int of int
  | Big of Bigint.t
  | Str of string
  | Data of ctor_desc * value array
  | Closure of closure

and closure = { code : code; captures : value array }

and code = {
  name : string;
  arity : int;
  n_locals : int;
  frame_size : int;
  instrs : instr array;
  spans : Diagnostic.span array;
}

and capture = FromLocal of int | FromCapture of int
and comparison = Less | LessEq | Greater | GreaterEq | Equal | NotEqual

and instr =
  | Push of value
  | GetLocal of int
  | SetLocal of int
  | GetCapture of int
  | GetGlobal of int * string
  | SetGlobal of int
  | Pop
  | Jump of int
  | JumpIfFalse of int
  | Switch of int array
  | MakeClosure of code * capture array
  | PatchCapture of { closure_slot : int; index : int; source_slot : int }
  | Call of int
  | TailCall of int
  | Return
  | MakeData of ctor_desc * int
  | GetField of int
  | AddLocalInt of int * int
  | SubLocalInt of int * int
  | JumpUnlessLocalInt of comparison * int * int * int
  | Add
  | Sub
  | Mul
  | Quot
  | Rem
  | Neg
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | Not
  | StrAppend
  | StrLength
  | Substring
  | Show
  | Print
  | Raise
  | MatchFailure

(* How many operands each instruction pops and pushes. *)
let stack_use = function
  | Push _ | GetLocal _ | GetCapture _ | GetGlobal _ | MakeClosure _ -> (0, 1)
  | AddLocalInt _ | SubLocalInt _ -> (0, 1)
  | JumpUnlessLocalInt _ -> (0, 0)
  | SetLocal _ | SetGlobal _ | Pop | JumpIfFalse _ | Switch _ -> (1, 0)
  | Jump _ | PatchCapture _ | MatchFailure -> (0, 0)
  | Call n -> (n + 1, 1)
  | TailCall n -> (n + 1, 0)
  | Return | Raise -> (1, 0)
  | MakeData (_, n) -> (n, 1)
  | GetField _ | Neg | Not | StrLength | Show | Print -> (1, 1)
  | Add | Sub | Mul | Quot | Rem | Eq | Ne | Lt | Le | Gt | Ge | StrAppend -> (2, 1)
  | Substring -> (3, 1)

let successors pc = function
  | Jump target -> [ target ]
  | JumpIfFalse target | JumpUnlessLocalInt (_, _, _, target) -> [ pc + 1; target ]
  | Switch targets -> Array.to_list targets
  | TailCall _ | Return | Raise | MatchFailure -> []
  | _ -> [ pc + 1 ]

let max_operand_depth instrs =
  let n = Array.length instrs in
  let depth_at = Array.make n (-1) in
  let deepest = ref 0 in
  let rec visit pc depth =
    if pc < 0 || pc >= n then failwith (Printf.sprintf "bytecode: pc %d out of range" pc)
    else if depth_at.(pc) >= 0 then (
      if depth_at.(pc) <> depth then
        failwith
          (Printf.sprintf "bytecode: pc %d reached with depths %d and %d" pc depth_at.(pc)
             depth))
    else
      let instr = instrs.(pc) in
      let pops, pushes = stack_use instr in
      if pops > depth then
        failwith (Printf.sprintf "bytecode: stack underflow at pc %d" pc);
      depth_at.(pc) <- depth;
      let after = depth - pops + pushes in
      deepest := max !deepest (max depth after);
      List.iter (fun next -> visit next after) (successors pc instr)
  in
  if n > 0 then visit 0 0;
  !deepest

(* Disassembly *)

let value_to_string = function
  | Unit -> "unit"
  | True -> "true"
  | False -> "false"
  | Undefined -> "<undefined>"
  | Int n -> string_of_int n
  | Big n -> Bigint.to_string n
  | Str s -> "\"" ^ String.escaped s ^ "\""
  | Data (c, _) -> c.name
  | Closure c -> "<function " ^ c.code.name ^ ">"

let capture_to_string = function
  | FromLocal i -> "local " ^ string_of_int i
  | FromCapture i -> "capture " ^ string_of_int i

let comparison_to_string = function
  | Less -> "<"
  | LessEq -> "<="
  | Greater -> ">"
  | GreaterEq -> ">="
  | Equal -> "="
  | NotEqual -> "!="

let instr_to_string = function
  | Push v -> "Push " ^ value_to_string v
  | GetLocal i -> Printf.sprintf "GetLocal %d" i
  | SetLocal i -> Printf.sprintf "SetLocal %d" i
  | GetCapture i -> Printf.sprintf "GetCapture %d" i
  | GetGlobal (i, name) -> Printf.sprintf "GetGlobal %d (%s)" i name
  | SetGlobal i -> Printf.sprintf "SetGlobal %d" i
  | Pop -> "Pop"
  | Jump t -> Printf.sprintf "Jump %d" t
  | JumpIfFalse t -> Printf.sprintf "JumpIfFalse %d" t
  | Switch targets ->
      let targets = Array.to_list targets |> List.map string_of_int in
      "Switch [" ^ String.concat " " targets ^ "]"
  | MakeClosure (code, captures) ->
      let captures = Array.to_list captures |> List.map capture_to_string in
      Printf.sprintf "MakeClosure %s [%s]" code.name (String.concat ", " captures)
  | PatchCapture { closure_slot; index; source_slot } ->
      Printf.sprintf "PatchCapture local %d, capture %d <- local %d" closure_slot index
        source_slot
  | Call n -> Printf.sprintf "Call %d" n
  | TailCall n -> Printf.sprintf "TailCall %d" n
  | Return -> "Return"
  | MakeData (c, n) -> Printf.sprintf "MakeData %s %d" c.name n
  | GetField i -> Printf.sprintf "GetField %d" i
  | AddLocalInt (slot, k) -> Printf.sprintf "AddLocalInt local %d, %d" slot k
  | SubLocalInt (slot, k) -> Printf.sprintf "SubLocalInt local %d, %d" slot k
  | JumpUnlessLocalInt (cmp, slot, k, target) ->
      Printf.sprintf "JumpUnlessLocalInt local %d %s %d, %d" slot
        (comparison_to_string cmp) k target
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Quot -> "Quot"
  | Rem -> "Rem"
  | Neg -> "Neg"
  | Eq -> "Eq"
  | Ne -> "Ne"
  | Lt -> "Lt"
  | Le -> "Le"
  | Gt -> "Gt"
  | Ge -> "Ge"
  | Not -> "Not"
  | StrAppend -> "StrAppend"
  | StrLength -> "StrLength"
  | Substring -> "Substring"
  | Show -> "Show"
  | Print -> "Print"
  | Raise -> "Raise"
  | MatchFailure -> "MatchFailure"

let disassemble code =
  let buf = Buffer.create 256 in
  let rec go code =
    Printf.bprintf buf "%s (arity %d, locals %d, frame %d)\n" code.name code.arity
      code.n_locals code.frame_size;
    Array.iteri
      (fun pc instr -> Printf.bprintf buf "%6d  %s\n" pc (instr_to_string instr))
      code.instrs;
    Buffer.add_char buf '\n';
    Array.iter (function MakeClosure (inner, _) -> go inner | _ -> ()) code.instrs
  in
  go code;
  Buffer.contents buf

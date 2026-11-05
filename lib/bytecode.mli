(** The VM's instruction set and runtime values.

    These live together because they are mutually recursive: closures hold code, and a
    [Push] instruction holds a value.

    The instructions ending in [LocalInt] are combined forms of very common sequences,
    like [GetLocal; Push; Sub]. Every stack write goes through the GC's write barrier, so
    doing the same work in fewer writes is the main way to make the VM faster.

    Each call gets a frame on one shared stack:
    {v
      [closure] [arg 0 ... arg n-1] [other locals ...] [operands ...]
                 ^ frame pointer
    v}
    Locals are addressed relative to the frame pointer. Operands are pushed above them. *)

type ctor_desc = { name : string; tag : int; is_list : bool }
(** What the runtime needs to know about a constructor. [is_list] exists only for printing
    lists as [(1 2 3)]. *)

type value =
  | Unit
  | True
  | False
  | Undefined
      (** A global that has not been initialized yet. Never visible to programs. *)
  | Int of int  (** Integers that fit in a native [int]. *)
  | Big of Bigint.t
      (** All other integers. Never holds a value that would fit in [Int]. *)
  | Str of string
  | Data of ctor_desc * value array
  | Closure of closure

and closure = { code : code; captures : value array }

and code = {
  name : string;
  arity : int;
  n_locals : int;  (** Includes the parameters. *)
  frame_size : int;  (** [n_locals] plus the deepest the operand stack ever gets. *)
  instrs : instr array;
  spans : Diagnostic.span array;  (** Parallel to [instrs], for runtime error locations. *)
}

and capture = FromLocal of int | FromCapture of int
and comparison = Less | LessEq | Greater | GreaterEq | Equal | NotEqual

and instr =
  | Push of value
  | GetLocal of int
  | SetLocal of int  (** Pops into the slot. *)
  | GetCapture of int
  | GetGlobal of int * string  (** The name is only for the error message. *)
  | SetGlobal of int
  | Pop
  | Jump of int
  | JumpIfFalse of int
  | Switch of int array  (** Pops a [Data] and jumps to the target for its tag. *)
  | MakeClosure of code * capture array
  | PatchCapture of { closure_slot : int; index : int; source_slot : int }
      (** Ties the knot for [letrec]: closures that refer to each other can only be
          completed after all of them exist. *)
  | Call of int
  | TailCall of int
      (** Reuses the current frame, so tail recursion runs in constant stack. *)
  | Return
  | MakeData of ctor_desc * int
  | GetField of int  (** Replaces a [Data] with one of its fields. *)
  | AddLocalInt of int * int  (** Pushes local slot + constant. *)
  | SubLocalInt of int * int  (** Pushes local slot - constant. *)
  | JumpUnlessLocalInt of comparison * int * int * int
      (** Compares local slot with a constant, and jumps to the target if that is false.
      *)
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
  | Raise  (** Pops a string and aborts with it. *)
  | MatchFailure  (** Unreachable if the exhaustiveness checker is correct. *)

val max_operand_depth : instr array -> int
(** Walks every path through the code to find the deepest the operand stack gets. Along
    the way it checks that the stack is never popped when empty, and that every path
    reaching an instruction arrives with the same depth. A compiler bug that breaks these
    would otherwise corrupt the VM's stack silently.
    @raise Failure if the code is malformed. *)

val instr_to_string : instr -> string

val disassemble : code -> string
(** Prints the code and, recursively, every function it creates. *)

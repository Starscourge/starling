open Bytecode

type frame = { fn_name : string; location : Diagnostic.span }
type error = { message : string; trace : frame list; hidden_calls : int }

exception Runtime_error of error
exception Halt of value

(* About 16 million slots, or 128 MB. Deep enough for millions of non-tail calls, while still
   stopping runaway recursion well before it exhausts memory. *)
let max_stack_slots = 1 lsl 24
let initial_stack_slots = 1 lsl 14
let initial_frames = 1 lsl 10

type t = {
  mutable globals : value array;
  mutable stack : value array;
  mutable frames : int array;
  out : string -> unit;
}

let create ?(out = print_string) () =
  {
    globals = Array.make 256 Undefined;
    stack = Array.make initial_stack_slots Unit;
    frames = Array.make (2 * initial_frames) 0;
    out;
  }

let grown_stack stack needed =
  if needed > max_stack_slots then raise (Value.Error "stack overflow");
  let rec size n = if n >= needed then n else size (2 * n) in
  let bigger = Array.make (min max_stack_slots (size (Array.length stack))) Unit in
  Array.blit stack 0 bigger 0 (Array.length stack);
  bigger

let grown_frames frames =
  let bigger = Array.make (2 * Array.length frames) 0 in
  Array.blit frames 0 bigger 0 (Array.length frames);
  bigger

let as_string = function Str s -> s | _ -> assert false
let of_bool b = if b then True else False

let substring s start len =
  match (start, len) with
  | Int start, Int len when start >= 0 && len >= 0 && start <= String.length s - len ->
      String.sub s start len
  | _ -> raise (Value.Error "substring: index out of range")

(* How many frames to report from each end of a deep stack. *)
let keep_each_end = 5

(* Frame 0 is the running function, and frame i > 0 is the caller saved at position
   [frame_count - i]. Only the reported frames get built, because a stack overflow can have
   millions of them. *)
let capture_trace stack frames frame_count current pc =
  let describe (code : code) pc =
    { fn_name = code.name; location = code.spans.(pc - 1) }
  in
  let frame_at index =
    if index = 0 then describe current pc
    else
      let saved = frame_count - index in
      match stack.(frames.((2 * saved) + 1) - 1) with
      | Closure c -> describe c.code frames.(2 * saved)
      | _ -> assert false
  in
  let total = frame_count + 1 in
  let range first last = List.init (last - first + 1) (fun i -> first + i) in
  if total <= 2 * keep_each_end then (List.map frame_at (range 0 (total - 1)), 0)
  else
    ( List.map frame_at
        (range 0 (keep_each_end - 1) @ range (total - keep_each_end) (total - 1)),
      total - (2 * keep_each_end) )

(* The whole interpreter is one function, and no closure captures its registers (sp, fp, pc and
   so on). That lets OCaml treat them as local variables rather than heap-allocated references,
   which makes dispatch noticeably faster.

   Frame layout:

     stack.(fp - 1)  the running closure
     stack.(fp + i)  local slot i, with parameters first
     above those     the operand stack, up to sp

   A saved frame is just the caller's pc and fp. The caller's code and captures come back from
   its closure at stack.(fp - 1), so a call writes no pointers to the frame array. *)
let comparison_holds cmp order =
  match cmp with
  | Less -> order < 0
  | LessEq -> order <= 0
  | Greater -> order > 0
  | GreaterEq -> order >= 0
  | Equal -> order = 0
  | NotEqual -> order <> 0

let run vm ~n_globals (main : code) =
  if n_globals > Array.length vm.globals then (
    let bigger = Array.make (2 * n_globals) Undefined in
    Array.blit vm.globals 0 bigger 0 (Array.length vm.globals);
    vm.globals <- bigger);
  let globals = vm.globals in
  let stack = ref vm.stack in
  let frames = ref vm.frames in
  let frame_count = ref 0 in
  let current = ref main in
  let instrs = ref main.instrs in
  let captures = ref [||] in
  let pc = ref 0 in
  let fp = ref 1 in
  let sp = ref (1 + main.n_locals) in
  if main.frame_size + 1 > Array.length !stack then
    stack := grown_stack !stack (main.frame_size + 1);
  !stack.(0) <- Closure { code = main; captures = [||] };
  try
    while true do
      (* pc can't leave the code: the stack checker verified every jump target, and every path
         through a function ends in a return, a tail call or an abort. *)
      let instr = Array.unsafe_get !instrs !pc in
      incr pc;
      let s = !stack in
      match instr with
      | Push v ->
          s.(!sp) <- v;
          incr sp
      | GetLocal i ->
          s.(!sp) <- s.(!fp + i);
          incr sp
      | SetLocal i ->
          decr sp;
          s.(!fp + i) <- s.(!sp)
      | GetCapture i ->
          s.(!sp) <- !captures.(i);
          incr sp
      | GetGlobal (i, name) -> (
          match globals.(i) with
          | Undefined ->
              raise
                (Value.Error (Printf.sprintf "'%s' was used before it was defined" name))
          | v ->
              s.(!sp) <- v;
              incr sp)
      | SetGlobal i ->
          decr sp;
          globals.(i) <- s.(!sp)
      | Pop -> decr sp
      | Jump target -> pc := target
      | JumpIfFalse target -> (
          decr sp;
          match s.(!sp) with False -> pc := target | _ -> ())
      | Switch targets -> (
          decr sp;
          match s.(!sp) with Data (c, _) -> pc := targets.(c.tag) | _ -> assert false)
      | MakeClosure (code, sources) ->
          let n = Array.length sources in
          let values =
            if n = 0 then [||]
            else
              let values = Array.make n Unit in
              for i = 0 to n - 1 do
                values.(i) <-
                  (match sources.(i) with
                  | FromLocal slot -> s.(!fp + slot)
                  | FromCapture index -> !captures.(index))
              done;
              values
          in
          s.(!sp) <- Closure { code; captures = values };
          incr sp
      | PatchCapture { closure_slot; index; source_slot } -> (
          match s.(!fp + closure_slot) with
          | Closure c -> c.captures.(index) <- s.(!fp + source_slot)
          | _ -> assert false)
      | Call n -> (
          let callee = !sp - n - 1 in
          match s.(callee) with
          | Closure c ->
              let code = c.code in
              let new_fp = callee + 1 in
              (* Checked before anything changes, so a stack overflow traces cleanly. *)
              if new_fp + code.frame_size > Array.length s then
                stack := grown_stack s (new_fp + code.frame_size);
              let fc = !frame_count in
              if (2 * fc) + 1 >= Array.length !frames then frames := grown_frames !frames;
              !frames.(2 * fc) <- !pc;
              !frames.((2 * fc) + 1) <- !fp;
              frame_count := fc + 1;
              fp := new_fp;
              sp := new_fp + code.n_locals;
              current := code;
              instrs := code.instrs;
              captures := c.captures;
              pc := 0
          | _ -> assert false)
      | TailCall n -> (
          let callee = !sp - n - 1 in
          match s.(callee) with
          | Closure c ->
              let code = c.code in
              (* Slide the callee and its arguments down over the current frame. The destination
                 is always below the source, so copying upward is safe. *)
              let base = !fp - 1 in
              for i = 0 to n do
                s.(base + i) <- s.(callee + i)
              done;
              if !fp + code.frame_size > Array.length s then
                stack := grown_stack s (!fp + code.frame_size);
              sp := !fp + code.n_locals;
              current := code;
              instrs := code.instrs;
              captures := c.captures;
              pc := 0
          | _ -> assert false)
      | Return -> (
          let result = s.(!sp - 1) in
          let base = !fp - 1 in
          s.(base) <- result;
          sp := base + 1;
          let fc = !frame_count in
          if fc = 0 then raise_notrace (Halt result);
          let fc = fc - 1 in
          frame_count := fc;
          pc := !frames.(2 * fc);
          fp := !frames.((2 * fc) + 1);
          match s.(!fp - 1) with
          | Closure c ->
              current := c.code;
              instrs := c.code.instrs;
              captures := c.captures
          | _ -> assert false)
      | MakeData (desc, n) ->
          let base = !sp - n in
          let fields =
            match n with
            | 1 -> [| s.(base) |]
            | 2 -> [| s.(base); s.(base + 1) |]
            | 3 -> [| s.(base); s.(base + 1); s.(base + 2) |]
            | _ -> Array.sub s base n
          in
          s.(base) <- Data (desc, fields);
          sp := base + 1
      | GetField i -> (
          match s.(!sp - 1) with
          | Data (_, fields) -> s.(!sp - 1) <- fields.(i)
          | _ -> assert false)
      (* The common all-small-integer cases are handled inline, and everything else goes through
         Value, which also handles overflow into Bigint. *)
      | AddLocalInt (slot, k) ->
          (match s.(!fp + slot) with
          | Int x when x lxor (x + k) land (k lxor (x + k)) >= 0 -> s.(!sp) <- Int (x + k)
          | v -> s.(!sp) <- Value.add v (Int k));
          incr sp
      | SubLocalInt (slot, k) ->
          (match s.(!fp + slot) with
          | Int x when x lxor k land (x lxor (x - k)) >= 0 -> s.(!sp) <- Int (x - k)
          | v -> s.(!sp) <- Value.sub v (Int k));
          incr sp
      | JumpUnlessLocalInt (cmp, slot, k, target) ->
          let holds =
            match s.(!fp + slot) with
            | Int x -> (
                match cmp with
                | Less -> x < k
                | LessEq -> x <= k
                | Greater -> x > k
                | GreaterEq -> x >= k
                | Equal -> x = k
                | NotEqual -> x <> k)
            | v -> comparison_holds cmp (Value.compare_ints v (Int k))
          in
          if not holds then pc := target
      | Add -> (
          decr sp;
          match (s.(!sp - 1), s.(!sp)) with
          | Int x, Int y when x lxor (x + y) land (y lxor (x + y)) >= 0 ->
              s.(!sp - 1) <- Int (x + y)
          | a, b -> s.(!sp - 1) <- Value.add a b)
      | Sub -> (
          decr sp;
          match (s.(!sp - 1), s.(!sp)) with
          | Int x, Int y when x lxor y land (x lxor (x - y)) >= 0 ->
              s.(!sp - 1) <- Int (x - y)
          | a, b -> s.(!sp - 1) <- Value.sub a b)
      | Mul ->
          decr sp;
          s.(!sp - 1) <- Value.mul s.(!sp - 1) s.(!sp)
      | Quot ->
          decr sp;
          s.(!sp - 1) <- Value.quot s.(!sp - 1) s.(!sp)
      | Rem ->
          decr sp;
          s.(!sp - 1) <- Value.rem s.(!sp - 1) s.(!sp)
      | Neg -> s.(!sp - 1) <- Value.neg s.(!sp - 1)
      | Eq -> (
          decr sp;
          match (s.(!sp - 1), s.(!sp)) with
          | Int x, Int y -> s.(!sp - 1) <- of_bool (x = y)
          | a, b -> s.(!sp - 1) <- of_bool (Value.equal a b))
      | Ne ->
          decr sp;
          s.(!sp - 1) <- of_bool (not (Value.equal s.(!sp - 1) s.(!sp)))
      | Lt -> (
          decr sp;
          match (s.(!sp - 1), s.(!sp)) with
          | Int x, Int y -> s.(!sp - 1) <- of_bool (x < y)
          | a, b -> s.(!sp - 1) <- of_bool (Value.compare_ints a b < 0))
      | Le -> (
          decr sp;
          match (s.(!sp - 1), s.(!sp)) with
          | Int x, Int y -> s.(!sp - 1) <- of_bool (x <= y)
          | a, b -> s.(!sp - 1) <- of_bool (Value.compare_ints a b <= 0))
      | Gt -> (
          decr sp;
          match (s.(!sp - 1), s.(!sp)) with
          | Int x, Int y -> s.(!sp - 1) <- of_bool (x > y)
          | a, b -> s.(!sp - 1) <- of_bool (Value.compare_ints a b > 0))
      | Ge -> (
          decr sp;
          match (s.(!sp - 1), s.(!sp)) with
          | Int x, Int y -> s.(!sp - 1) <- of_bool (x >= y)
          | a, b -> s.(!sp - 1) <- of_bool (Value.compare_ints a b >= 0))
      | Not -> (
          match s.(!sp - 1) with
          | False -> s.(!sp - 1) <- True
          | _ -> s.(!sp - 1) <- False)
      | StrAppend ->
          decr sp;
          s.(!sp - 1) <- Str (as_string s.(!sp - 1) ^ as_string s.(!sp))
      | StrLength -> s.(!sp - 1) <- Int (String.length (as_string s.(!sp - 1)))
      | Substring ->
          sp := !sp - 2;
          s.(!sp - 1) <- Str (substring (as_string s.(!sp - 1)) s.(!sp) s.(!sp + 1))
      | Show -> s.(!sp - 1) <- Str (Value.display s.(!sp - 1))
      | Print ->
          vm.out (Value.display s.(!sp - 1));
          vm.out "\n";
          s.(!sp - 1) <- Unit
      | Raise ->
          decr sp;
          raise (Value.Error (as_string s.(!sp)))
      | MatchFailure -> raise (Value.Error "no clause of this match applies")
    done;
    assert false
  with
  | Halt result ->
      vm.stack <- !stack;
      vm.frames <- !frames;
      result
  | (Value.Error _ | Stack_overflow) as cause ->
      let message =
        match cause with
        | Value.Error message -> message
        | _ -> "stack overflow while comparing or printing deeply nested data"
      in
      vm.stack <- !stack;
      vm.frames <- !frames;
      let trace, hidden_calls = capture_trace !stack !frames !frame_count !current !pc in
      raise (Runtime_error { message; trace; hidden_calls })

let render_error { message; trace; hidden_calls } =
  let buf = Buffer.create 128 in
  Printf.bprintf buf "runtime error: %s\n" message;
  List.iteri
    (fun i { fn_name; location } ->
      if hidden_calls > 0 && i = keep_each_end then
        Printf.bprintf buf "  ... %d more calls ...\n" hidden_calls;
      if location == Diagnostic.none then Printf.bprintf buf "  in %s\n" fn_name
      else Printf.bprintf buf "  in %s at %s\n" fn_name (Diagnostic.location location))
    trace;
  Buffer.contents buf

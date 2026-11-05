open Bytecode

(* The code generator's state for one function. *)
type fn = {
  mutable instrs : instr array;
  mutable spans : Diagnostic.span array;
  mutable len : int;
  mutable next_slot : int;
  mutable max_slot : int;
  slots : (int, int) Hashtbl.t;  (** Variable id to local slot. *)
  captures : (int, int) Hashtbl.t;  (** Variable id to capture index. *)
  mutable pending_fails : (int -> unit) list;
      (** Jumps waiting for the target of the enclosing [Fatbar]. *)
}

let new_fn () =
  {
    instrs = Array.make 32 Pop;
    spans = Array.make 32 Diagnostic.none;
    len = 0;
    next_slot = 0;
    max_slot = 0;
    slots = Hashtbl.create 16;
    captures = Hashtbl.create 8;
    pending_fails = [];
  }

let emit ?(span = Diagnostic.none) fn instr =
  if fn.len = Array.length fn.instrs then (
    let grow a filler =
      let bigger = Array.make (2 * Array.length a) filler in
      Array.blit a 0 bigger 0 (Array.length a);
      bigger
    in
    fn.instrs <- grow fn.instrs Pop;
    fn.spans <- grow fn.spans Diagnostic.none);
  fn.instrs.(fn.len) <- instr;
  fn.spans.(fn.len) <- span;
  fn.len <- fn.len + 1

(* Emits a jump whose target isn't known yet, and returns a function that fills it in. *)
let emit_forward fn make =
  let at = fn.len in
  emit fn (make (-1));
  fun target -> fn.instrs.(at) <- make target

let alloc_slot fn (v : Core.var) =
  let slot = fn.next_slot in
  fn.next_slot <- slot + 1;
  fn.max_slot <- max fn.max_slot fn.next_slot;
  Hashtbl.replace fn.slots v.id slot;
  slot

let capture_source fn (v : Core.var) =
  match Hashtbl.find_opt fn.slots v.id with
  | Some slot -> FromLocal slot
  | None -> (
      match Hashtbl.find_opt fn.captures v.id with
      | Some index -> FromCapture index
      | None -> failwith ("compile: unresolved variable " ^ v.name))

let get_var fn v =
  match capture_source fn v with
  | FromLocal slot -> emit fn (GetLocal slot)
  | FromCapture index -> emit fn (GetCapture index)

let rec compile_function ?(entry_point = false) (l : Core.lambda) =
  let free = Core.free_vars l in
  let fn = new_fn () in
  List.iteri (fun i (v : Core.var) -> Hashtbl.replace fn.captures v.id i) free;
  List.iter (fun p -> ignore (alloc_slot fn p)) l.params;
  (* The entry point never tail-calls, so <main> stays in stack traces. *)
  if entry_point then (
    expr fn ~tail:false l.body;
    emit fn Return)
  else expr fn ~tail:true l.body;
  let instrs = Array.sub fn.instrs 0 fn.len in
  let code =
    {
      name = l.fn_name;
      arity = List.length l.params;
      n_locals = fn.max_slot;
      frame_size = fn.max_slot + max_operand_depth instrs;
      instrs;
      spans = Array.sub fn.spans 0 fn.len;
    }
  in
  (code, free)

(* Emits code that creates a closure and leaves it on the stack. *)
and emit_closure fn (l : Core.lambda) =
  let code, free = compile_function l in
  let sources = List.map (capture_source fn) free in
  emit fn (MakeClosure (code, Array.of_list sources));
  free

(* After [expr fn ~tail:false e] runs, the value of [e] is on the stack. After
   [expr fn ~tail:true e], the function has returned on every path. *)
and expr fn ~tail (e : Core.expr) =
  let finish () = if tail then emit fn Return in
  match e with
  | Const v ->
      emit fn (Push v);
      finish ()
  | Local v ->
      get_var fn v;
      finish ()
  | Global (slot, name, span) ->
      emit ~span fn (GetGlobal (slot, name));
      finish ()
  | SetGlobal (slot, rhs) ->
      expr fn ~tail:false rhs;
      emit fn (SetGlobal slot);
      emit fn (Push Unit);
      finish ()
  | Lambda l ->
      ignore (emit_closure fn l);
      finish ()
  | Call (f, args, span) ->
      expr fn ~tail:false f;
      List.iter (expr fn ~tail:false) args;
      let n = List.length args in
      emit ~span fn (if tail then TailCall n else Call n)
  | Prim (instr, args, span) ->
      List.iter (expr fn ~tail:false) args;
      emit ~span fn instr;
      finish ()
  | MakeData (desc, args) ->
      List.iter (expr fn ~tail:false) args;
      emit fn (MakeData (desc, List.length args));
      finish ()
  | Field (v, index) ->
      get_var fn v;
      emit fn (GetField index);
      finish ()
  | If (c, a, b) ->
      expr fn ~tail:false c;
      let to_else = emit_forward fn (fun t -> JumpIfFalse t) in
      expr fn ~tail a;
      if tail then (
        to_else fn.len;
        expr fn ~tail b)
      else
        let to_end = emit_forward fn (fun t -> Jump t) in
        to_else fn.len;
        expr fn ~tail b;
        to_end fn.len
  | Let (v, rhs, body) ->
      expr fn ~tail:false rhs;
      let saved = fn.next_slot in
      let slot = alloc_slot fn v in
      emit fn (SetLocal slot);
      expr fn ~tail body;
      fn.next_slot <- saved
  | LetRec (bindings, body) ->
      let saved = fn.next_slot in
      let slots = List.map (fun (v, _) -> alloc_slot fn v) bindings in
      (* Create every closure first. A capture of a sibling that doesn't exist yet reads a
         stale slot, which is harmless because the next step overwrites it. *)
      let frees =
        List.map2
          (fun (_, l) slot ->
            let free = emit_closure fn l in
            emit fn (SetLocal slot);
            free)
          bindings slots
      in
      let is_sibling (v : Core.var) =
        List.exists (fun ((s : Core.var), _) -> s.id = v.id) bindings
      in
      List.iter2
        (fun free closure_slot ->
          List.iteri
            (fun index (v : Core.var) ->
              if is_sibling v then
                emit fn
                  (PatchCapture
                     { closure_slot; index; source_slot = Hashtbl.find fn.slots v.id }))
            free)
        frees slots;
      expr fn ~tail body;
      fn.next_slot <- saved
  | Seq (a, b) ->
      expr fn ~tail:false a;
      emit fn Pop;
      expr fn ~tail b
  | Switch (v, cases, n_ctors) ->
      get_var fn v;
      let targets = Array.make n_ctors (-1) in
      emit fn (Switch targets);
      for tag = 0 to n_ctors - 1 do
        if not (List.mem_assoc tag cases) then
          fn.pending_fails <- (fun t -> targets.(tag) <- t) :: fn.pending_fails
      done;
      let to_end =
        List.map
          (fun (tag, body) ->
            targets.(tag) <- fn.len;
            expr fn ~tail body;
            if tail then None else Some (emit_forward fn (fun t -> Jump t)))
          cases
      in
      List.iter (Option.iter (fun patch -> patch fn.len)) to_end
  | Fatbar (a, b) ->
      let outer = fn.pending_fails in
      fn.pending_fails <- [];
      expr fn ~tail a;
      let to_end = if tail then None else Some (emit_forward fn (fun t -> Jump t)) in
      List.iter (fun patch -> patch fn.len) fn.pending_fails;
      fn.pending_fails <- outer;
      expr fn ~tail b;
      Option.iter (fun patch -> patch fn.len) to_end
  | Fail ->
      let patch = emit_forward fn (fun t -> Jump t) in
      fn.pending_fails <- patch :: fn.pending_fails
  | MatchFailure span -> emit ~span fn MatchFailure

let program body =
  let code, free =
    compile_function ~entry_point:true { fn_name = "<main>"; params = []; body }
  in
  assert (free = []);
  code

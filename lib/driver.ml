type session = { mutable env : Infer.env; mutable globals : Lower.globals; vm : Vm.t }

type outcome = {
  value : Bytecode.value;
  value_type : Types.scheme option;
  definitions : (string * Types.scheme) list;
}

let parse source = Parse.program (Reader.read source)

let compile session source =
  let checked = Infer.check_program session.env (parse source) in
  let globals, body = Lower.program session.globals checked.items in
  (checked, globals, Compile.program body)

let eval session source =
  let checked, globals, code = compile session source in
  (* Commit before running. A runtime failure leaves earlier definitions in the unit available,
     and any it never reached report that they were used before being defined. *)
  session.env <- checked.env;
  session.globals <- globals;
  let value = Vm.run session.vm ~n_globals:(Lower.global_count globals) code in
  { value; value_type = checked.last_type; definitions = checked.definitions }

let create ?out () =
  let session =
    { env = Infer.initial_env; globals = Lower.no_globals; vm = Vm.create ?out () }
  in
  ignore (eval session { name = "<prelude>"; text = Prelude_source.text });
  session

let check session source = (Infer.check_program session.env (parse source)).definitions

let type_of session (source : Diagnostic.source) =
  match Reader.read source with
  | [ form ] -> Infer.type_of_expr session.env (Parse.expr form)
  | _ ->
      Diagnostic.error
        { source; start = 0; stop = String.length source.text }
        "expected exactly one expression"

let disassemble session source =
  let _, _, code = compile session source in
  Bytecode.disassemble code

open Starling

let version = "0.1.0"

let usage =
  "Starling: a statically typed functional language.\n\n\
   Usage:\n\
  \  starling run FILE      compile and run a program\n\
  \  starling check FILE    type-check a program and print each definition's type\n\
  \  starling disasm FILE   print a program's bytecode without running it\n\
  \  starling repl          start an interactive session\n\
  \  starling --version     print the version\n"

let repl_help =
  "Enter an expression or definition. Input can span several lines.\n\n\
  \  :type EXPR     show the type of an expression without running it\n\
  \  :disasm EXPR   show the bytecode for an expression\n\
  \  :help          show this message\n\
  \  :quit          leave (Ctrl-D also works)\n"

let color_stderr = lazy (Unix.isatty Unix.stderr)

let report_diagnostic diagnostic =
  flush stdout;
  prerr_string (Diagnostic.render ~color:(Lazy.force color_stderr) diagnostic);
  flush stderr

let too_deep_message = "starling: this program is too deeply nested to compile\n"

let report_runtime_error error =
  flush stdout;
  prerr_string (Vm.render_error error);
  flush stderr

let read_file path =
  try In_channel.with_open_bin path In_channel.input_all
  with Sys_error message ->
    prerr_endline ("starling: " ^ message);
    exit 2

let load path : Diagnostic.source = { name = path; text = read_file path }

(* Runs a one-file command, and turns the two kinds of program error into exit code 1. *)
let with_errors_reported f =
  try f () with
  | Diagnostic.Error diagnostic ->
      report_diagnostic diagnostic;
      exit 1
  | Vm.Runtime_error error ->
      report_runtime_error error;
      exit 1
  | Stack_overflow ->
      prerr_string too_deep_message;
      exit 1

let run_file path =
  with_errors_reported (fun () -> ignore (Driver.eval (Driver.create ()) (load path)))

let print_definition (name, scheme) =
  Printf.printf "%s : %s\n" name (Types.scheme_to_string scheme)

let check_file path =
  with_errors_reported (fun () ->
      List.iter print_definition (Driver.check (Driver.create ()) (load path)))

let disassemble_file path =
  with_errors_reported (fun () ->
      print_string (Driver.disassemble (Driver.create ()) (load path)))

(* The REPL *)

let repl_source text : Diagnostic.source = { name = "<repl>"; text }

let print_outcome (outcome : Driver.outcome) =
  List.iter print_definition outcome.definitions;
  match (outcome.value, outcome.value_type) with
  | Bytecode.Unit, _ | _, None -> ()
  | value, Some scheme ->
      Printf.printf "%s : %s\n" (Value.repr value) (Types.scheme_to_string scheme)

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let command_argument ~command line =
  String.trim
    (String.sub line (String.length command) (String.length line - String.length command))

let handle_line session line =
  let trimmed = String.trim line in
  try
    if trimmed = ":help" then print_string repl_help
    else if starts_with ~prefix:":type" trimmed then
      let text = command_argument ~command:":type" trimmed in
      print_endline (Types.scheme_to_string (Driver.type_of session (repl_source text)))
    else if starts_with ~prefix:":disasm" trimmed then
      let text = command_argument ~command:":disasm" trimmed in
      print_string (Driver.disassemble session (repl_source text))
    else if starts_with ~prefix:":" trimmed then
      Printf.printf "unknown command %s (try :help)\n" trimmed
    else print_outcome (Driver.eval session (repl_source line))
  with
  | Diagnostic.Error diagnostic -> report_diagnostic diagnostic
  | Vm.Runtime_error error -> report_runtime_error error
  | Stack_overflow ->
      prerr_string too_deep_message;
      flush stderr

(* Reads until the input is a complete form, so a definition can span several lines. *)
let rec read_input prompt acc =
  print_string prompt;
  flush stdout;
  match input_line stdin with
  | exception End_of_file -> None
  | line ->
      let text = if acc = "" then line else acc ^ "\n" ^ line in
      let is_command = starts_with ~prefix:":" (String.trim text) in
      if is_command || Reader.is_complete text then Some text else read_input "... " text

let repl () =
  Printf.printf "Starling %s. Type :help for help and :quit to leave.\n" version;
  let session = Driver.create () in
  let rec loop () =
    match read_input "> " "" with
    | None -> print_newline ()
    | Some text when String.trim text = ":quit" -> ()
    | Some text when String.trim text = "" -> loop ()
    | Some text ->
        handle_line session text;
        flush stdout;
        loop ()
  in
  loop ()

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [ "run"; path ] -> run_file path
  | [ "check"; path ] -> check_file path
  | [ "disasm"; path ] -> disassemble_file path
  | [ "repl" ] -> repl ()
  | [ ("--version" | "-v") ] -> print_endline version
  | [ ("--help" | "-h") ] -> print_string usage
  | _ ->
      prerr_string usage;
      exit 2

(* Golden tests: each .sl program has a .expected file holding exactly what it should print. A
   failure means behavior changed, either by mistake or on purpose. If on purpose, regenerate the
   file and review the diff. *)

open Starling

let read_file path = In_channel.with_open_bin path In_channel.input_all

let programs_in directory =
  Sys.readdir directory |> Array.to_list
  |> List.filter (fun name -> Filename.check_suffix name ".sl")
  |> List.sort String.compare

(* Runs a program as the command line does, collecting printed output and any error report. *)
let run_program path =
  let buf = Buffer.create 256 in
  (try
     let session = Driver.create ~out:(Buffer.add_string buf) () in
     ignore (Driver.eval session { name = path; text = read_file path })
   with
  | Diagnostic.Error diagnostic -> Buffer.add_string buf (Diagnostic.render diagnostic)
  | Vm.Runtime_error error -> Buffer.add_string buf (Vm.render_error error));
  Buffer.contents buf

let golden_test ~run_from directory name =
  let test () =
    let cwd = Sys.getcwd () in
    Fun.protect
      ~finally:(fun () -> Sys.chdir cwd)
      (fun () ->
        Sys.chdir run_from;
        let path = Filename.concat directory name in
        let expected = read_file (Filename.chop_suffix path ".sl" ^ ".expected") in
        Alcotest.(check string) name expected (run_program path))
  in
  Alcotest.test_case name `Quick test

let tests_for ~run_from directory =
  programs_in (Filename.concat run_from directory)
  |> List.map (golden_test ~run_from directory)

let examples = tests_for ~run_from:".." "examples"
let errors = tests_for ~run_from:"." "errors"

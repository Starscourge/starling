type source = { name : string; text : string }
type span = { source : source; start : int; stop : int }
type t = { message : string; span : span; notes : string list }

exception Error of t

let error ?(notes = []) span message = raise (Error { message; span; notes })
let errorf ?notes span fmt = Printf.ksprintf (fun msg -> error ?notes span msg) fmt

let merge a b =
  { source = a.source; start = min a.start b.start; stop = max a.stop b.stop }

let none = { source = { name = "<generated>"; text = "" }; start = 0; stop = 0 }

let line_col source offset =
  let offset = min offset (String.length source.text) in
  let line = ref 1 and last_newline = ref (-1) in
  for i = 0 to offset - 1 do
    if source.text.[i] = '\n' then (
      incr line;
      last_newline := i)
  done;
  (!line, offset - !last_newline)

let location span =
  let line, col = line_col span.source span.start in
  Printf.sprintf "%s:%d:%d" span.source.name line col

(* The text of the line containing [offset]. *)
let line_at text offset =
  let len = String.length text in
  let offset = min offset len in
  let start =
    match String.rindex_from_opt text (max 0 (offset - 1)) '\n' with
    | Some i when offset > 0 -> i + 1
    | _ -> 0
  in
  let stop =
    match String.index_from_opt text start '\n' with Some i -> i | None -> len
  in
  String.sub text start (stop - start)

let render ?(color = false) { message; span; notes } =
  let paint code s = if color then "\027[" ^ code ^ "m" ^ s ^ "\027[0m" else s in
  let red = paint "1;31" and bold = paint "1" and blue = paint "1;34" in
  let buf = Buffer.create 128 in
  Buffer.add_string buf (red "error" ^ bold (": " ^ message) ^ "\n");
  if span.source.text <> "" then (
    let line, col = line_col span.source span.start in
    let gutter = String.length (string_of_int line) in
    let pad = String.make gutter ' ' in
    let text = line_at span.source.text span.start in
    (* A span crossing lines is underlined only up to the end of its first line. *)
    let width = max 1 (min (span.stop - span.start) (String.length text - (col - 1))) in
    (* Tabs become single spaces so the caret column lines up in any terminal. *)
    let text = String.map (function '\t' -> ' ' | c -> c) text in
    Printf.bprintf buf "%s %s:%d:%d\n" (blue (pad ^ "-->")) span.source.name line col;
    Printf.bprintf buf "%s\n" (blue (pad ^ " |"));
    Printf.bprintf buf "%s %s\n" (blue (Printf.sprintf "%d |" line)) text;
    Printf.bprintf buf "%s %s%s\n"
      (blue (pad ^ " |"))
      (String.make (col - 1) ' ')
      (red (String.make width '^')));
  List.iter (fun note -> Printf.bprintf buf "%s %s\n" (blue "note:") note) notes;
  Buffer.contents buf

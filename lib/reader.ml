type sexp = { node : node; span : Diagnostic.span }
and node = Int of Bigint.t | Str of string | Sym of string | List of sexp list

let rec read_form lx (token, span) =
  match (token : Lexer.token) with
  | Int n -> { node = Int n; span }
  | Str s -> { node = Str s; span }
  | Sym s -> { node = Sym s; span }
  | Quote -> (
      match Lexer.next lx with
      | Eof, _ ->
          raise
            (Lexer.Incomplete
               { message = "expected a form after the quote"; span; notes = [] })
      | next ->
          let quoted = read_form lx next in
          {
            node = List [ { node = Sym "quote"; span }; quoted ];
            span = Diagnostic.merge span quoted.span;
          })
  | LParen -> read_list lx span []
  | RParen -> Diagnostic.error span "unexpected ')'"
  | Eof -> assert false

and read_list lx open_span acc =
  match Lexer.next lx with
  | RParen, close_span ->
      { node = List (List.rev acc); span = Diagnostic.merge open_span close_span }
  | Eof, _ ->
      raise (Lexer.Incomplete { message = "unclosed '('"; span = open_span; notes = [] })
  | next -> read_list lx open_span (read_form lx next :: acc)

let read_all source =
  let lx = Lexer.create source in
  let rec go acc =
    match Lexer.next lx with
    | Eof, _ -> List.rev acc
    | next -> go (read_form lx next :: acc)
  in
  go []

let read source =
  try read_all source
  with Lexer.Incomplete diagnostic -> raise (Diagnostic.Error diagnostic)

let is_complete text =
  match read_all { name = "<input>"; text } with
  | _ -> true
  | exception Lexer.Incomplete _ -> false
  | exception Diagnostic.Error _ -> true

let rec to_string { node; _ } =
  match node with
  | Int n -> Bigint.to_string n
  | Str s -> "\"" ^ String.escaped s ^ "\""
  | Sym s -> s
  | List items -> "(" ^ String.concat " " (List.map to_string items) ^ ")"

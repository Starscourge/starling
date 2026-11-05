type token =
  | LParen
  | RParen
  | Quote
  | Int of Bigint.t
  | Str of string
  | Sym of string
  | Eof

type t = { source : Diagnostic.source; mutable pos : int }

exception Incomplete of Diagnostic.t

let create source = { source; pos = 0 }
let span lx start : Diagnostic.span = { source = lx.source; start; stop = lx.pos }
let at_end lx = lx.pos >= String.length lx.source.text
let peek lx = lx.source.text.[lx.pos]
let is_digit c = c >= '0' && c <= '9'

let is_symbol_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | '+' | '-' | '*' | '/' | '<' | '>' | '=' | '!' | '?' | '_' | '%' | '&' | '^' | '~'
  | ':' | '.' | '$' | '#' | '@' ->
      true
  | _ -> false

let rec skip_trivia lx =
  if not (at_end lx) then
    match peek lx with
    | ' ' | '\t' | '\r' | '\n' ->
        lx.pos <- lx.pos + 1;
        skip_trivia lx
    | ';' ->
        while (not (at_end lx)) && peek lx <> '\n' do
          lx.pos <- lx.pos + 1
        done;
        skip_trivia lx
    | _ -> ()

let scan_string lx =
  let start = lx.pos in
  lx.pos <- lx.pos + 1;
  let buf = Buffer.create 16 in
  let unterminated () =
    raise
      (Incomplete { message = "unterminated string"; span = span lx start; notes = [] })
  in
  let rec go () =
    if at_end lx then unterminated ();
    let c = peek lx in
    lx.pos <- lx.pos + 1;
    match c with
    | '"' -> ()
    | '\\' ->
        if at_end lx then unterminated ();
        let escape = peek lx in
        lx.pos <- lx.pos + 1;
        (match escape with
        | 'n' -> Buffer.add_char buf '\n'
        | 't' -> Buffer.add_char buf '\t'
        | 'r' -> Buffer.add_char buf '\r'
        | '\\' -> Buffer.add_char buf '\\'
        | '"' -> Buffer.add_char buf '"'
        | other ->
            Diagnostic.errorf
              (span lx (lx.pos - 2))
              "unknown escape sequence '\\%c'" other);
        go ()
    | c ->
        Buffer.add_char buf c;
        go ()
  in
  go ();
  (Str (Buffer.contents buf), span lx start)

let scan_atom lx =
  let start = lx.pos in
  while (not (at_end lx)) && is_symbol_char (peek lx) do
    lx.pos <- lx.pos + 1
  done;
  let text = String.sub lx.source.text start (lx.pos - start) in
  let here = span lx start in
  let looks_numeric =
    is_digit text.[0]
    || (String.length text > 1 && (text.[0] = '-' || text.[0] = '+') && is_digit text.[1])
  in
  if looks_numeric then
    match Bigint.of_string_opt text with
    | Some n -> (Int n, here)
    | None -> Diagnostic.errorf here "malformed number '%s'" text
  else (Sym text, here)

let next lx =
  skip_trivia lx;
  if at_end lx then (Eof, span lx lx.pos)
  else
    let start = lx.pos in
    let single token =
      lx.pos <- lx.pos + 1;
      (token, span lx start)
    in
    match peek lx with
    | '(' -> single LParen
    | ')' -> single RParen
    | '\'' -> single Quote
    | '"' -> scan_string lx
    | c when is_symbol_char c -> scan_atom lx
    | c ->
        lx.pos <- lx.pos + 1;
        Diagnostic.errorf (span lx start) "unexpected character '%s'" (Char.escaped c)

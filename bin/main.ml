        (* File main.ml  *)

let rec print_string_list l=
  match l with
  | hd::tl -> print_string hd; print_string_list tl
  | [] -> print_newline()

let _ =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  let lexer  = Sedlexing.with_tokenizer Lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  let result = parser lexer in
    print_string_list result; flush stdout


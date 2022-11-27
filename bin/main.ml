open Ast.Print_ast
open Semantic_analysis.Type_checks

let _ =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  let lexer  = Sedlexing.with_tokenizer Lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  let result = parser lexer in (
    match result with
    | Program(_, _, stmts) -> (type_check_stmts (stmts) (None)); print_ast result
  )
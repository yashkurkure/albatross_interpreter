open Print_ast
open Semantic_analysis.Type_checks
open Semantic_analysis.Symbol_resolution


let _ =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  let lexer  = Sedlexing.with_tokenizer Lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  let result = parser lexer in (
  let (symbol_table, function_table) = symbol_resolution result in
    type_check result symbol_table function_table; 
    print_ast result symbol_table function_table;
  )
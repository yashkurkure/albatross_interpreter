open Semantic_analysis.Type_checks
open Semantic_analysis.Symbol_resolution
open Program_evaluation.Evaluation
open Program_evaluation.Intrinsics

let _ =
  let file = (match Sys.argv with | [|_; f |] -> f | _ -> Printf.eprintf "Usage: ./albatrossin.exe <program_file>\n"; exit(1)) in
  let fchannel = open_in file in
  let lexbuf = Sedlexing.Utf8.from_channel fchannel in
  let lexer  = Sedlexing.with_tokenizer Lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  let result_parse = parser lexer in
  let result = register_intrinsics result_parse in (
  let (symbol_table, function_table) = symbol_resolution result in
    type_check result symbol_table function_table;
    eval result function_table ;
    print_string "\n";
  )
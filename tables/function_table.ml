open Ast. Ast_nodes

(* A table that maps function names to Ast function declaration nodes*)
type functiontab = string -> fundec_node option

let empty_functiontab = fun _ -> None

let lookup_functiontab (function_table: functiontab) (x: string): fundec_node option = function_table x

(* Update/Add a value to the function table*)
let update_functiontab (function_table: functiontab) (x: string) (f: fundec_node) = fun y -> if y = x then Some(f) else function_table y
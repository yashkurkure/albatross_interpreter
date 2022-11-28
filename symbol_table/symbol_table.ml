open Ast. Ast_nodes

type symtab = string -> ty_node option

let empty_symtab = fun _ -> None

let lookup_symtab (symbol_table: symtab) (x: string): ty_node option = symbol_table x

let update_symtab (symbol_table: symtab) (x: string) (t: ty_node) = fun y -> if y = x then Some(t) else symbol_table y

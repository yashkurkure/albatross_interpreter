open Ast.Ast_nodes

let register_intrinsics (p: program): program = 
  match p with
  | Program(_, _, _) -> p
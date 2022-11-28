open Ast.Ast_nodes

type fun_frame = FunFrame of fundec_node

let rec create_fun_frames(fundec:fundec_node list): fun_frame list = 
  match fundec with
  | hd::rest -> FunFrame(hd)::create_fun_frames(rest)
  | [] -> []

  

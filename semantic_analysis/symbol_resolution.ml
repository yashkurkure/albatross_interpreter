open Ast.Ast_nodes
open Symbol_table

let rec symbol_resolution_exprs (exprs: exp_node list) (f: fun_frame option): unit =
  match exprs with
  | expr::rest -> symbol_resolution_expr expr f; symbol_resolution_exprs rest f
  | [] -> assert(false)

and symbol_resolution_expr (exp: exp_node) (_: fun_frame option): unit = 
  match exp with
  | Ident(_) -> assert(true)
  | _ -> assert(false);;


let rec symbol_resolution_stmts (stmts: stmt_node list) (f: fun_frame option): unit = 
  match stmts with
  | stmt::rest -> symbol_resolution_stmt stmt f; symbol_resolution_stmts rest f
  | [] -> assert(true)

and symbol_resolution_stmt (stmt: stmt_node) (_: fun_frame option): unit = 
  match stmt with
  | Return(_) -> assert(true)
  | _ -> assert(true);;

let rec symbol_resolution_vars (vars: vardec_node list) (symbol_table: symtab) (f: fun_frame option): symtab = 
  match vars with
  | vardec::rest -> symbol_resolution_vars (rest) (symbol_resolution_var (vardec) (symbol_table) (f)) (f)
  | [] -> symbol_table
and symbol_resolution_var (var: vardec_node) (symbol_table: symtab) (_: fun_frame option): symtab = 
  match var with VarDec(ident, t, _) -> (match lookup_symtab symbol_table ident with
                                        | None -> update_symtab symbol_table ident t
                                        | _ -> print_string "Attempting to define duplicate variable\n";exit(3) (*Attempting to define duplicate variable*)
                                        );;


let symbol_resolution (p: program): symtab = 
  match p with
  | Program(vardecs, _, stmts) -> (
    let symbol_table = symbol_resolution_vars vardecs empty_symtab None in 
    symbol_resolution_stmts stmts None; symbol_table
  )
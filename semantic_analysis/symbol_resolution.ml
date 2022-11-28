open Ast.Ast_nodes
open Symbol_table

let rec symbol_resolution_exprs (exprs: exp_node list) (symbol_table: symtab) (f: fun_frame option): unit =
  match exprs with
  | expr::rest -> symbol_resolution_expr expr symbol_table f; symbol_resolution_exprs rest symbol_table f
  | [] -> assert(false)

and symbol_resolution_expr (exp: exp_node) (symbol_table: symtab) (f: fun_frame option): unit = 
  match exp with
  | Ident(x) -> (match lookup_symtab symbol_table x with
                | None -> exit(3) (* Fail when undeclared symbol used in expression*)
                | _ -> assert(true))
  | Add(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | Sub(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | Mul(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | Div(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | Rem(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | BinOr(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | BinAnd(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | Xor(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | Or(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | And(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | Leq(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | Geq(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | Less(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | Greater(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | Neq(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | Eq(e1, e2) -> symbol_resolution_expr e1 symbol_table f; symbol_resolution_expr e2 symbol_table f
  | Not(e) -> symbol_resolution_expr e symbol_table f
  | Int(_) -> assert(true)
  | String(_) -> assert(true)
  | FunCallExp(_, _) -> assert(false)
  | Nil -> assert(true)





let rec symbol_resolution_stmts (stmts: stmt_node list) (symbol_table: symtab) (f: fun_frame option): unit = 
  match stmts with
  | stmt::rest -> symbol_resolution_stmt stmt symbol_table f; symbol_resolution_stmts rest symbol_table f
  | [] -> assert(true)

and symbol_resolution_stmt (stmt: stmt_node) (symbol_table: symtab) (f: fun_frame option): unit = 
  match stmt with
  | Return(exp) -> symbol_resolution_expr exp symbol_table f  
  | _ -> assert(true);;

let rec symbol_resolution_vars (vars: vardec_node list) (symbol_table: symtab) (f: fun_frame option): symtab = 
  match vars with
  | vardec::rest -> symbol_resolution_vars (rest) (symbol_resolution_var (vardec) (symbol_table) (f)) (f)
  | [] -> symbol_table
and symbol_resolution_var (var: vardec_node) (symbol_table: symtab) (f: fun_frame option): symtab = 
  match var with VarDec(ident, t, e) -> symbol_resolution_expr e symbol_table f;
                                        (match lookup_symtab symbol_table ident with
                                        | None -> update_symtab symbol_table ident t
                                        | _ -> print_string "Attempting to define duplicate variable\n";exit(3) (*Attempting to define duplicate variable*)
                                        );;


let symbol_resolution (p: program): symtab = 
  match p with
  | Program(vardecs, _, stmts) -> (
    let symbol_table = symbol_resolution_vars vardecs empty_symtab None in 
    symbol_resolution_stmts stmts symbol_table None; symbol_table
  )
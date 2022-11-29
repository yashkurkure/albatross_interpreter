open Ast.Ast_nodes
open Ast.Context
open Tables.Symbol_table
open Tables.Function_table

let rec symbol_resolution_exprs (exprs: exp_node list) (symbol_table: symtab) (function_table: functiontab) (c: context): unit =
  match exprs with
  | expr::rest -> symbol_resolution_expr expr symbol_table function_table c; symbol_resolution_exprs rest symbol_table function_table c
  | [] -> assert(true)

and symbol_resolution_expr (exp: exp_node) (symbol_table: symtab) (function_table: functiontab) (c: context): unit = 
  match exp with
  | Ident(x) -> (match lookup_symtab symbol_table x with
                | None -> exit(3) (* Fail when undeclared symbol used in expression*)
                | _ -> assert(true))
  | Add(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | Sub(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | Mul(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | Div(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | Rem(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | BinOr(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | BinAnd(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | Xor(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | Or(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | And(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | Leq(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | Geq(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | Less(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | Greater(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | Neq(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | Eq(e1, e2) -> symbol_resolution_expr e1 symbol_table function_table c; symbol_resolution_expr e2 symbol_table function_table c
  | Not(e) -> symbol_resolution_expr e symbol_table function_table c
  | Int(_) -> assert(true)
  | String(_) -> assert(true)
  | FunCallExp(x, arg_inits) -> (match (lookup_functiontab function_table x) with
                                | None -> exit(3)
                                | Some(_) -> (symbol_resolution_exprs arg_inits symbol_table function_table c);
                                )
  | Nil -> assert(true)





let rec symbol_resolution_stmts (stmts: stmt_node list) (symbol_table: symtab) (function_table: functiontab) (c: context): unit = 
  match stmts with
  | stmt::rest -> symbol_resolution_stmt stmt symbol_table function_table c; symbol_resolution_stmts rest symbol_table function_table c
  | [] -> assert(true)

and symbol_resolution_stmt (stmt: stmt_node) (symbol_table: symtab) (function_table: functiontab) (c: context): unit = 
  match stmt with
  | Return(exp) -> symbol_resolution_expr exp symbol_table function_table c
  | _ -> assert(true);;

let rec symbol_resolution_vars (vars: vardec_node list) (symbol_table: symtab) (function_table: functiontab) (c: context): symtab = 
  match vars with
  | vardec::rest -> symbol_resolution_vars (rest) (symbol_resolution_var (vardec) (symbol_table) (function_table) (c)) (function_table) (c)
  | [] -> symbol_table
and symbol_resolution_var (var: vardec_node) (symbol_table: symtab) (function_table: functiontab) (c: context): symtab = 
  match var with VarDec(ident, t, e) -> symbol_resolution_expr e symbol_table function_table c;
                                        (match lookup_symtab symbol_table ident with
                                        | None -> update_symtab symbol_table ident t
                                        | _ -> print_string "Attempting to define duplicate variable\n";exit(3) (*Attempting to define duplicate variable*)
                                        );;


(* Adds the arguemnts of the function to the symbol table *)
let rec add_function_args_symtab (funargs: fundec_arg list) (symbol_table: symtab): symtab = 
  match funargs with
  | FunDecArg(x,t)::rest -> (match lookup_symtab symbol_table x with
                            | Some(_) -> exit(3) (* Fail when arg has the same name as some global variable*)
                            | None -> add_function_args_symtab (rest) (update_symtab symbol_table x t))
  | [] -> symbol_table


let rec symbol_resolution_functions (funs: fundec_node list) (symbol_table: symtab) (function_table: functiontab): functiontab = 
  match funs with
  | FunDec(x,t,args,vars,stmts)::rest -> (match (lookup_functiontab function_table x) with
                                          | Some(_) -> exit(3) (* Fail when duplicate function is defined*)
                                          | None -> symbol_resolution_functions 
                                                    (rest) 
                                                    (symbol_table) 
                                                        (symbol_resolution_function 
                                                            (FunDec(x,t,args,vars,stmts)) 
                                                            (symbol_table) 
                                                            (update_functiontab 
                                                                (function_table) 
                                                                (x)
                                                                (FunDec(x,t,args,vars,stmts)) 
                                                            )
                                                          )
                                          )
  | [] -> function_table
and symbol_resolution_function (fundec: fundec_node) (symbol_table:symtab) (function_table: functiontab): functiontab = 
  match fundec with FunDec(x,_,args,vars,stmts)-> (
    let args_sym_table = add_function_args_symtab args symbol_table in
    let args_locs_sym_table = symbol_resolution_vars vars args_sym_table function_table (Func_ct(x)) in
    symbol_resolution_stmts stmts args_locs_sym_table function_table (Func_ct(x))
  ) ; function_table;;


let symbol_resolution (p: program): symtab*functiontab = 
  match p with
  | Program(vardecs, fundecs, stmts) -> (
    let symbol_table = symbol_resolution_vars vardecs empty_symtab empty_functiontab Glob_ct in 
    let function_table = symbol_resolution_functions fundecs symbol_table empty_functiontab in 
    symbol_resolution_stmts stmts symbol_table function_table Glob_ct; (symbol_table, function_table)
  )
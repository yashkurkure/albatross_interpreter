open Ast.Ast_nodes
open Ast.Context
open Tables.Symbol_table
open Tables.Function_table

(* Perform symbol resolution on a list of expressions. *)
let rec symbol_resolution_exprs (exprs: exp_node list) (symbol_table: symtab) (function_table: functiontab) (c: context): unit =
  match exprs with
  | expr::rest -> symbol_resolution_expr expr symbol_table function_table c; symbol_resolution_exprs rest symbol_table function_table c
  | [] -> assert(true)

and symbol_resolution_expr (exp: exp_node) (symbol_table: symtab) (function_table: functiontab) (c: context): unit = 
  match exp with
  | Ident(x) -> (match lookup_symtab symbol_table x with
                | None -> Printf.eprintf "[SymRes] Undeclared symbol %s used in expression.\n" x;exit(3) (* Fail when undeclared symbol used in expression*)
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
                                | None -> Printf.eprintf "[SymRes] Undeclared symbol %s called as a function.\n" x; exit(3)
                                | Some(_) -> (symbol_resolution_exprs arg_inits symbol_table function_table c);
                                )
  | Nil -> assert(true);;


(* Perform symbol resolution on a list of statements. *)
let rec symbol_resolution_stmts (stmts: stmt_node list) (symbol_table: symtab) (function_table: functiontab) (c: context): unit = 
  match stmts with
  | stmt::rest -> symbol_resolution_stmt stmt symbol_table function_table c; symbol_resolution_stmts rest symbol_table function_table c
  | [] -> assert(true) (* End of list *)

and symbol_resolution_stmt (stmt: stmt_node) (symbol_table: symtab) (function_table: functiontab) (c: context): unit = 
  match stmt with
  | Return(exp) -> symbol_resolution_expr exp symbol_table function_table c
  | IfThenElse(exp, thn, el) -> symbol_resolution_expr exp symbol_table function_table c; 
                                symbol_resolution_stmts thn symbol_table function_table c; 
                                symbol_resolution_stmts el symbol_table function_table c;
  | WhileOtherwise(exp, body, other) -> symbol_resolution_expr exp symbol_table function_table c; 
                                symbol_resolution_stmts body symbol_table function_table c; 
                                symbol_resolution_stmts other symbol_table function_table c;
  | Repeat(exp, body) -> symbol_resolution_expr exp symbol_table function_table c; 
                                symbol_resolution_stmts body symbol_table function_table c; 
  | Assign(ident, exp) -> (match lookup_symtab symbol_table ident with
                            | Some(_) -> assert(true)
                            | None -> Printf.eprintf "[SymRes] Undeclared symbol %s in assign statement.\n" ident; exit(3));
                          symbol_resolution_expr exp symbol_table function_table c
  | FunCallStmt(f, arg_inits) -> (match lookup_functiontab function_table f with
                                  | Some(_) -> assert(true)
                                  | None -> Printf.eprintf "[SymRes] Undeclared symbol %s called as a function.\n" f; exit(3));
                                symbol_resolution_exprs arg_inits symbol_table function_table c;;


(* Perform symbol resolution on a list of variable declarations. *)
let rec symbol_resolution_vars (vars: vardec_node list) (symbol_table: symtab) (function_table: functiontab) (c: context): symtab = 
  match vars with
  | vardec::rest -> symbol_resolution_vars (rest) (symbol_resolution_var (vardec) (symbol_table) (function_table) (c)) (function_table) (c)
  | [] -> symbol_table
and symbol_resolution_var (var: vardec_node) (symbol_table: symtab) (function_table: functiontab) (c: context): symtab = 
  match var with VarDec(ident, t, e) -> symbol_resolution_expr e symbol_table function_table c;
                                        (match lookup_symtab symbol_table ident with
                                        | None -> update_symtab symbol_table ident t
                                        | _ -> Printf.eprintf "[SymRes] Attempting to define duplicate variable %s.\n" ident; exit(3) (*Attempting to define duplicate variable*)
                                        );;


(* Adds the arguments of the function to the symbol table. 
    This is always called before performing symbol resolution on the function's body. *)
let rec add_function_args_symtab (f: string) (funargs: fundec_arg list) (symbol_table: symtab): symtab = 
  match funargs with
  | FunDecArg(x,t)::rest -> (match lookup_symtab symbol_table x with
                            | Some(_) -> Printf.eprintf "[SymRes] Duplicate symbol %s in the arguments of function %s.\n" x f;exit(3) (* Fail when arg has the same name as some global variable*)
                            | None -> add_function_args_symtab f (rest) (update_symtab symbol_table x t))
  | [] -> symbol_table


(* Perform symbol on a list of function declarations. *)
let rec symbol_resolution_functions (funs: fundec_node list) (symbol_table: symtab) (function_table: functiontab): functiontab = 
  match funs with
  | FunDec(x,t,args,vars,stmts)::rest -> (match (lookup_functiontab function_table x) with
                                          | Some(_) -> Printf.eprintf "[SymRes] Duplicate function %s defined.\n" x; exit(3) (* Fail when duplicate function is defined*)
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

    (*Add the args to the symbol table.*)
    let args_sym_table = add_function_args_symtab x args symbol_table in
    (*Add the vairables to the symbol table.*)
    let args_locs_sym_table = symbol_resolution_vars vars args_sym_table function_table (Func_ct(x)) in
    (*Perform symbol resolution on the function body.*)
    symbol_resolution_stmts stmts args_locs_sym_table function_table (Func_ct(x))
  ) ; function_table;;


(* Perform symbol resolution on the AST program. *)
let symbol_resolution (p: program): symtab*functiontab = 
  match p with
  | Program(vardecs, fundecs, stmts) -> (
    (* Get the symbol table after performing symbol resolution on the variable declarations.*)
    let symbol_table = symbol_resolution_vars vardecs empty_symtab empty_functiontab Glob_ct in
    (* Get the functtion table after performing symbol resolution on the function declarations.*)
    let function_table = symbol_resolution_functions fundecs symbol_table empty_functiontab in
    (* Perform symbol resolution on the global statements. *)
    symbol_resolution_stmts stmts symbol_table function_table Glob_ct; 
    (*Return the symbol table and the function table for type checkings. *)
    (symbol_table, function_table)
  )
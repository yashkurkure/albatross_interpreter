open Ast.Ast_nodes
open Ast.Context
open Tables.Symbol_table
open Tables.Function_table





let rec type_check_expr (exp: exp_node)(symbol_table: symtab)(function_table: functiontab)(c: context): ty_node = 
  match exp, c with
  | String(_), _ -> String_ty
  | Int(_), _-> Int_ty
  | Add(e1, e2),_ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | Sub(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | Mul(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | Div(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | Rem(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | BinOr(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | BinAnd(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | Xor(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | Or(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | And(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | Leq(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | Geq(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | Less(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | Greater(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | Neq(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | Eq(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table function_table c
  | Not(e),_ -> (match (type_check_expr e symbol_table function_table c) with 
              | Int_ty -> Int_ty
              | _ -> exit(3) (* Fail when operand is not Int_ty*)
              )
  | Ident(ident), _ -> (match lookup_symtab symbol_table ident with
                      | None -> exit(3) (* Fail when identifier is undefined in the symbol table*)
                      | Some(t) -> t)
  | FunCallExp(x, arg_inits), c -> (match lookup_functiontab function_table x with 
                                    |Some(FunDec(_,t,arg_decs,_,_)) -> type_check_funargs arg_inits arg_decs symbol_table function_table c;t 
                                    | None -> exit(3))
  | Nil, _ -> Void_ty

  and type_check_binop_expr (e1: exp_node) (e2: exp_node) (symbol_table: symtab) (function_table: functiontab) (c: context): ty_node= 
    match (type_check_expr e1 symbol_table function_table c), (type_check_expr e2 symbol_table function_table c) with
                       | Int_ty, Int_ty -> Int_ty
                       | _, _ -> exit(3) (* Fail when both operands are not Int_ty*)

and  type_check_funargs (arg_inits: exp_node list) (arg_decs: fundec_arg list)(symbol_table: symtab)(function_table: functiontab)(c:context): unit =
match arg_inits, arg_decs with
| exp::rest_init, FunDecArg(_, t)::rest_arg_decs -> (match type_check_expr exp symbol_table function_table c with 
                                                    | t2 -> if t = t2 then assert(true) else exit(3));
                                                    type_check_funargs rest_init rest_arg_decs symbol_table function_table c
| [], [] -> assert(true)
| _, _ -> exit(3)


let rec type_check_stmt (stmt: stmt_node)(symbol_table: symtab)(function_table: functiontab)(c: context): unit = 
  match stmt, c with
  | Return(exp), c -> (match (type_check_expr exp symbol_table function_table c), c with
                    | String_ty, Glob_ct -> exit(3) (* Fail when top level returns String_ty *)
                    | Int_ty, Glob_ct -> assert(true)
                    | Void_ty, Glob_ct -> exit(3)
                    | t1, Func_ct(x)-> (match lookup_functiontab function_table x with
                                        | Some(FunDec(_,t2,_,_,_)) -> if t2 = t1 then assert(true) else exit(3) (* Fail when return exp type is not same as function's return type*)
                                        | _ -> assert(true)))
  | IfThenElse(cond, thn, els), c -> (match (type_check_expr cond symbol_table function_table c) with 
                                     | Int_ty -> assert(true)
                                     | _ -> exit(3));  (* Fail when guard is not Int_ty *)
                                     ignore(type_check_stmts(thn)(symbol_table)(function_table)(c));
                                     ignore(type_check_stmts(els)(symbol_table)(function_table)(c))

  | WhileOtherwise(cond, body, otherwise), c -> (match (type_check_expr cond symbol_table function_table c) with 
                                                | Int_ty -> assert(true)
                                                | _ -> exit(3));  (* Fail when guard is not Int_ty *)
                                                ignore(type_check_stmts(body)(symbol_table)(function_table)(c));
                                                ignore(type_check_stmts(otherwise)(symbol_table)(function_table)(c))

  | Repeat(times, body), c -> (match (type_check_expr times symbol_table function_table c) with 
                              | Int_ty -> assert(true)
                              | _ -> exit(3));  (* Fail when times is not Int_ty *)
                              ignore(type_check_stmts(body)(symbol_table)(function_table)(c))
  
  | Assign(ident, exp), c -> (match (lookup_symtab symbol_table ident), (type_check_expr exp symbol_table function_table c) with
                            | Some(t1), t2 -> if t1 = t2 then assert(true) else exit(3) (* Fail when type of rhs != lhs*)
                            | None, _ -> exit(3)(* Fail when a symbol is assigned before declaration *)
                            )
  | FunCallStmt(ident, arg_inits), c -> (match lookup_functiontab function_table ident with 
                                |Some(FunDec(_,_,arg_decs,_,_)) -> type_check_funargs arg_inits arg_decs symbol_table function_table c;
                                | None -> exit(3))

and type_check_stmts (stmts: stmt_node list)(symbol_table: symtab)(function_table: functiontab)(c: context): unit = 
  match stmts, c with
  | stmt::rest, c -> ignore(type_check_stmt(stmt)(symbol_table)(function_table)(c)); type_check_stmts(rest)(symbol_table)(function_table)(c)
  | [], _ -> assert(true);;


let rec type_check_var (var: vardec_node) (symbol_table: symtab) (function_table: functiontab) (c: context): unit = 
  match var,c  with 
  | VarDec(_,_,_), Func_ct(_)-> assert(false)
  | VarDec(_,t,exp), Glob_ct-> (if t = (type_check_expr exp symbol_table function_table c) 
                              then assert(true) 
                              else exit(3) (* Fail when variable type and initial expression are not of same type*)
                            )

and type_check_vars (vars: vardec_node list) (symbol_table: symtab) (function_table: functiontab) (c: context): unit = 
  match vars, c with
  | var::rest, c -> ignore(type_check_var var symbol_table function_table c); type_check_vars rest symbol_table function_table c
  | [], _ -> assert(true);;


(* Adds the arguemnts of the function to the symbol table *)
let rec add_function_args_symtab (funargs: fundec_arg list) (symbol_table: symtab): symtab = 
  match funargs with
  | FunDecArg(x,t)::rest -> add_function_args_symtab (rest) (update_symtab symbol_table x t)
  | [] -> symbol_table;;

let rec add_function_locs_symtab (vardecs: vardec_node list) (symbol_table: symtab): symtab = 
  match vardecs with
  | VarDec(x, t, _)::rest -> add_function_locs_symtab (rest) (update_symtab symbol_table x t)
  | [] -> symbol_table;;


let rec type_check_functions (fundecs: fundec_node list) (symbol_table: symtab) (function_table: functiontab): unit = 
  match fundecs with
  | FunDec(x, _, args, vars, stmts)::rest -> (
    let args_sym_table = add_function_args_symtab args symbol_table in (
      let args_locs_sym_table = add_function_locs_symtab vars args_sym_table in (
        type_check_vars vars args_locs_sym_table function_table (Func_ct(x));
        type_check_stmts stmts args_locs_sym_table function_table  (Func_ct(x))
      )
    )
  ); type_check_functions rest symbol_table function_table
  | [] -> assert(true) 


let type_check (p: program) (symbol_table: symtab) (function_table: functiontab): unit = 
  match p with
  | Program(vars, funcs, stmts) -> type_check_vars vars symbol_table function_table Glob_ct; type_check_functions funcs symbol_table function_table; type_check_stmts stmts symbol_table function_table Glob_ct
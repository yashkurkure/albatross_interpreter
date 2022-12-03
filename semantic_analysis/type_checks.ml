open Ast.Ast_nodes
open Ast.Context
open Tables.Symbol_table
open Tables.Function_table


(* Type check an expression *)
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
              | _ -> (Printf.printf "[TypChk] Integer operand exprected in NOT expression.\n";assert(false)) (* Fail when operand is not Int_ty*)
              )
  | Ident(ident), _ -> (match lookup_symtab symbol_table ident with
                      | None -> (Printf.printf "[TypChk] Unexpected variable symbol slipped through symbol resolution.\n"; assert(false)) (* Control should not reach here after symbol resolution *)
                      | Some(t) -> t)
  | FunCallExp(x, arg_inits), c -> (match lookup_functiontab function_table x with 
                                    |Some(FunDec(_,t,arg_decs,_,_)) -> type_check_funargs arg_inits arg_decs symbol_table function_table c;t 
                                    | None -> (Printf.printf "[TypChk] Unexpected function symbol slipped through symbol resolution.\n"; assert(false)))
  | Nil, _ -> Void_ty

  and type_check_binop_expr (e1: exp_node) (e2: exp_node) (symbol_table: symtab) (function_table: functiontab) (c: context): ty_node= 
    match (type_check_expr e1 symbol_table function_table c), (type_check_expr e2 symbol_table function_table c) with
                       | Int_ty, Int_ty -> Int_ty
                       | _, _ -> (Printf.printf "[TypChk] Integer operands expected for binary operator.\n"; assert(false)) (* Fail when both operands are not Int_ty*)

and  type_check_funargs (arg_inits: exp_node list) (arg_decs: fundec_arg list)(symbol_table: symtab)(function_table: functiontab)(c:context): unit =
match arg_inits, arg_decs with
| exp::rest_init, FunDecArg(_, t)::rest_arg_decs -> (match type_check_expr exp symbol_table function_table c with 
                                                    | t2 -> if t = t2 then assert(true) else assert(false));
                                                    type_check_funargs rest_init rest_arg_decs symbol_table function_table c
| [], [] -> assert(true)
| _, _ -> (Printf.printf "[TypChk] Unexpected number of arguments given for function call.\n"; assert(false))


let rec type_check_stmt (stmt: stmt_node)(symbol_table: symtab)(function_table: functiontab)(c: context): unit = 
  match stmt, c with
  | Return(exp), c -> (match (type_check_expr exp symbol_table function_table c), c with
                    | String_ty, Glob_ct -> (Printf.printf "[TypChk] String cannot be returned at global level.\n"; assert(false)) (* Fail when top level returns String_ty *)
                    | Int_ty, Glob_ct -> assert(true)
                    | Void_ty, Glob_ct -> assert(false)
                    | t1, Func_ct(x)-> (match lookup_functiontab function_table x with
                                        | Some(FunDec(x,t2,_,_,_)) -> if t2 = t1 
                                                                      then assert(true) 
                                                                      else (Printf.printf "[TypChk] Function %s must return type %s, found %s.\n" x (ty_node_to_str t2) (ty_node_to_str t1); assert(false)) (* Fail when return exp type is not same as function's return type*)
                                        | _ -> assert(true)))
  | IfThenElse(cond, thn, els), c -> (match (type_check_expr cond symbol_table function_table c) with 
                                     | Int_ty -> assert(true)
                                     | _ -> (Printf.printf "[TypChk] ift statement expects an int expression as gaurd.\n"; assert(false)));  (* Fail when guard is not Int_ty *)
                                     ignore(type_check_stmts(thn)(symbol_table)(function_table)(c));
                                     ignore(type_check_stmts(els)(symbol_table)(function_table)(c))

  | WhileOtherwise(cond, body, otherwise), c -> (match (type_check_expr cond symbol_table function_table c) with 
                                                | Int_ty -> assert(true)
                                                | _ -> (Printf.printf "[TypChk] while statement expects an int expression as gaurd.\n"; assert(false)));  (* Fail when guard is not Int_ty *)
                                                ignore(type_check_stmts(body)(symbol_table)(function_table)(c));
                                                ignore(type_check_stmts(otherwise)(symbol_table)(function_table)(c))

  | Repeat(times, body), c -> (match (type_check_expr times symbol_table function_table c) with 
                              | Int_ty -> assert(true)
                              | _ -> (Printf.printf "[TypChk] repeat statement expects an int expression as gaurd.\n"; assert(false)));  (* Fail when times is not Int_ty *)
                              ignore(type_check_stmts(body)(symbol_table)(function_table)(c))
  
  | Assign(ident, exp), c -> (match (lookup_symtab symbol_table ident), (type_check_expr exp symbol_table function_table c) with
                            | Some(t1), t2 -> if t1 = t2 then assert(true) else (Printf.printf "[TypChk] assign statment LHS type not equal to RHS\n"; assert(false)) (* Fail when type of rhs != lhs*)
                            | None, _ -> (Printf.printf "[TypChk] Assignment of symbol %s failed symbol resolution\n" ident; assert(false))(* Fail when a symbol is assigned before declaration *)
                            )
  | FunCallStmt(ident, arg_inits), c -> (match lookup_functiontab function_table ident with 
                                |Some(FunDec(_,_,arg_decs,_,_)) -> type_check_funargs arg_inits arg_decs symbol_table function_table c;
                                | None -> (Printf.printf "[TypChk] Undefined function symbol %s slipped symbol resolution\n" ident; assert(false)))

and type_check_stmts (stmts: stmt_node list)(symbol_table: symtab)(function_table: functiontab)(c: context): unit = 
  match stmts, c with
  | stmt::rest, c -> ignore(type_check_stmt(stmt)(symbol_table)(function_table)(c)); type_check_stmts(rest)(symbol_table)(function_table)(c)
  | [], _ -> assert(true);;


let rec type_check_var (var: vardec_node) (symbol_table: symtab) (function_table: functiontab) (c: context): unit = 
  match var,c  with 
  | VarDec(_,t,exp), Func_ct(_)-> if t = (type_check_expr exp symbol_table function_table c) then assert(true) else assert(false)
  | VarDec(x,t,exp), Glob_ct-> let t2 = (type_check_expr exp symbol_table function_table c) in (if t = t2
                              then assert(true) 
                              else (Printf.printf "[TypChk] Vaiable declaration for %s of type %s does not match type %s of initializing expression\n" x (ty_node_to_str t) (ty_node_to_str t2); assert(false)) (* Fail when variable type and initial expression are not of same type*)
                            )

and type_check_vars (vars: vardec_node list) (symbol_table: symtab) (function_table: functiontab) (c: context): unit = 
  match vars, c with
  | var::rest, c -> ignore(type_check_var var symbol_table function_table c); type_check_vars rest symbol_table function_table c
  | [], _ -> assert(true);;


(* Adds function arguemnts of the function to the symbol table *)
let rec add_function_args_symtab (funargs: fundec_arg list) (symbol_table: symtab): symtab = 
  match funargs with
  | FunDecArg(x,t)::rest -> add_function_args_symtab (rest) (update_symtab symbol_table x t)
  | [] -> symbol_table;;

(* Add function variables to the symbol table *)
let rec add_function_locs_symtab (vardecs: vardec_node list) (symbol_table: symtab): symtab = 
  match vardecs with
  | VarDec(x, t, _)::rest -> add_function_locs_symtab (rest) (update_symtab symbol_table x t)
  | [] -> symbol_table;;


(* Type check a list of function declarations. *)
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


(* Type check program AST. *)
let type_check (p: program) (symbol_table: symtab) (function_table: functiontab): unit = 
  match p with
  | Program(vars, funcs, stmts) -> type_check_vars vars symbol_table function_table Glob_ct; type_check_functions funcs symbol_table function_table; type_check_stmts stmts symbol_table function_table Glob_ct
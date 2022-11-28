open Ast.Ast_nodes
open Symbol_table

let rec type_check_expr (exp: exp_node)(symbol_table: symtab)(f: fun_frame option): ty_node = 
  match exp, f with
  | String(_), _ -> String_ty
  | Int(_), _-> Int_ty
  | Add(e1, e2),_ -> (match (type_check_expr e1 symbol_table f), (type_check_expr e2 symbol_table f) with
                     | Int_ty, Int_ty -> Int_ty
                     | _, _ -> exit(3))
  | Sub(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | Mul(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | Div(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | Rem(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | BinOr(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | BinAnd(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | Xor(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | Or(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | And(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | Leq(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | Geq(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | Less(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | Greater(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | Neq(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | Eq(e1, e2), _ -> type_check_binop_expr e1 e2 symbol_table f
  | Not(e),_ -> (match (type_check_expr e symbol_table f) with 
              | Int_ty -> Int_ty
              | _ -> exit(3) (* Fail when operand is not Int_ty*)
              )
  | Ident(ident), _ -> (match lookup_symtab symbol_table ident with
                      | None -> exit(3) (* Fail when identifier is undefined in the symbol table*)
                      | Some(t) -> t)
  | _, _ -> assert(false)

  and type_check_binop_expr (e1: exp_node) (e2: exp_node) (symbol_table: symtab) (f: fun_frame option): ty_node= 
    match (type_check_expr e1 symbol_table f), (type_check_expr e2 symbol_table f) with
                       | Int_ty, Int_ty -> Int_ty
                       | _, _ -> exit(3) (* Fail when both operands are not Int_ty*)

and  type_check_exprs (exprs: exp_node list)(symbol_table: symtab)(f: fun_frame option) = 
  match exprs, f with
  | exp::rest, f -> ignore(type_check_expr (exp)(symbol_table)(f)); type_check_exprs(rest)(symbol_table)(f)
  | [], _ -> assert(true);;


let rec type_check_stmt (stmt: stmt_node)(symbol_table: symtab)(f: fun_frame option) = 
  match stmt, f with
  | Return(exp), f -> (match (type_check_expr exp symbol_table f), f with
                    | String_ty, None -> exit(3) (* Fail when top level returns String_ty *)
                    | Int_ty, None -> assert(true)
                    | t1, Some(FunFrame(FunDec(_,t2,_,_,_))) -> if t1 = t2 then assert(true) else exit(3)
                    | _, _ -> assert(false))

  | IfThenElse(cond, thn, els), _ -> (match (type_check_expr cond symbol_table f) with 
                                     | Int_ty -> assert(true)
                                     | _ -> exit(3));  (* Fail when guard is not Int_ty *)
                                     ignore(type_check_stmts(thn)(symbol_table)(f));
                                     ignore(type_check_stmts(els)(symbol_table)(f))

  | WhileOtherwise(cond, body, otherwise), _ -> (match (type_check_expr cond symbol_table f) with 
                                                | Int_ty -> assert(true)
                                                | _ -> exit(3));  (* Fail when guard is not Int_ty *)
                                                ignore(type_check_stmts(body)(symbol_table)(f));
                                                ignore(type_check_stmts(otherwise)(symbol_table)(f))

  | Repeat(times, body), _ -> (match (type_check_expr times symbol_table f) with 
                              | Int_ty -> assert(true)
                              | _ -> exit(3));  (* Fail when times is not Int_ty *)
                              ignore(type_check_stmts(body)(symbol_table)(f))
  
  | Assign(ident, exp), _ -> (match (lookup_symtab symbol_table ident), (type_check_expr exp symbol_table f) with
                            | Some(t1), t2 -> if t1 = t2 then assert(true) else exit(3) (* Fail when type of rhs != lhs*)
                            | None, _ -> exit(3)(* Fail when a symbol is assigned before declaration *)
                            )

  | _, _ -> assert(false)

and type_check_stmts (stmts: stmt_node list)(symbol_table: symtab)(f: fun_frame option): unit = 
  match stmts, f with
  | stmt::rest, f -> ignore(type_check_stmt(stmt)(symbol_table)(f)); type_check_stmts(rest)(symbol_table)(f)
  | [], _ -> assert(true);;


let rec type_check_var (var: vardec_node) (symbol_table: symtab) (f: fun_frame option): unit = 
  match var, f with 
  | VarDec(_,_,_), Some(FunFrame(FunDec(_,_,_,_,_))) -> assert(false)
  | VarDec(_,t,exp), None -> (if t = (type_check_expr exp symbol_table None) 
                              then assert(true) 
                              else exit(3) (* Fail when variable type and initial expression are not of same type*)
                            )

and type_check_vars (vars: vardec_node list) (symbol_table: symtab) (f: fun_frame option): unit = 
  match vars, f with
  | var::rest, f -> ignore(type_check_var var symbol_table f); type_check_vars rest symbol_table f
  | [], _ -> assert(true);;


let type_check (p: program) (symbol_table: symtab): unit = 
  match p with
  | Program(vars, _, stmts) -> type_check_vars vars symbol_table None; type_check_stmts stmts symbol_table None
open Ast.Ast_nodes

let rec type_check_expr (exp: exp_node)(f: fun_frame option): ty_node option = 
  match exp, f with
  | String(_), _ -> Some(String_ty)
  | Int(_), _-> Some(Int_ty)
  | Add(e1, e2),_ -> (match (type_check_expr e1 f), (type_check_expr e2 f) with
                     | Some(Int_ty), Some(Int_ty) -> Some(Int_ty)
                     | _, _ -> exit(3))
  | Sub(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | Mul(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | Div(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | Rem(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | BinOr(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | BinAnd(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | Xor(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | Or(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | And(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | Leq(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | Geq(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | Less(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | Greater(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | Neq(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | Eq(e1, e2), _ -> type_check_binop_expr e1 e2 f
  | Not(e),_ -> (match (type_check_expr e f) with 
              | Some(Int_ty) -> Some(Int_ty)
              | _ -> exit(3) (* Fail when operand is not Int_ty*)
              )
  |_,_-> None

  and type_check_binop_expr (e1: exp_node) (e2: exp_node) (f: fun_frame option): ty_node option = 
    match (type_check_expr e1 f), (type_check_expr e2 f) with
                       | Some(Int_ty), Some(Int_ty) -> Some(Int_ty)
                       | _, _ -> exit(3);; (* Fail when both operands are not Int_ty*)


let rec type_check_exprs (exprs: exp_node list)(f: fun_frame option) = 
  match exprs, f with
  | exp::rest, f -> ignore(type_check_expr (exp)(f)); type_check_exprs(rest)(f)
  | [], _ -> assert(true);;


let rec type_check_stmt (stmt: stmt_node)(f: fun_frame option) = 
  match stmt, f with
  | Return(exp), f -> (match (type_check_expr exp f), f with
                    | Some(String_ty), None -> exit(3) (* Fail when top level returns String_ty *)
                    | Some(Int_ty), None -> assert(true)
                    | Some(t1), Some(FunFrame(FunDec(_,t2,_,_,_))) -> if t1 = t2 then assert(true) else exit(3)
                    | _, _ -> assert(false))

  | IfThenElse(cond, thn, els), _ -> (match (type_check_expr cond f) with 
                                     | Some(Int_ty) -> assert(true)
                                     | _ -> exit(3));  (* Fail when guard is not Int_ty *)
                                     ignore(type_check_stmts(thn)(f));
                                     ignore(type_check_stmts(els)(f))

  | WhileOtherwise(cond, body, otherwise), _ -> (match (type_check_expr cond f) with 
                                                | Some(Int_ty) -> assert(true)
                                                | _ -> exit(3));  (* Fail when guard is not Int_ty *)
                                                ignore(type_check_stmts(body)(f));
                                                ignore(type_check_stmts(otherwise)(f))

  | Repeat(times, body), _ -> (match (type_check_expr times f) with 
                              | Some(Int_ty) -> assert(true)
                              | _ -> exit(3));  (* Fail when times is not Int_ty *)
                              ignore(type_check_stmts(body)(f))

  | _, _ -> assert(false)

and type_check_stmts (stmts: stmt_node list)(f: fun_frame option): unit = 
  match stmts, f with
  | stmt::rest, f -> ignore(type_check_stmt(stmt)(f)); type_check_stmts(rest)(f)
  | [], _ -> assert(true);;

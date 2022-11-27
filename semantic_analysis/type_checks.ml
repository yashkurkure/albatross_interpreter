open Ast.Ast_nodes

let rec type_check_expr (exp: exp_node)(f: fun_frame option): ty_node option = 
  match exp, f with
  | String(_), _ -> Some(String_ty)
  | Int(_), _-> Some(Int_ty)
  | Add(e1, e2),_ -> (match (type_check_expr e1 f), (type_check_expr e2 f) with
                     | Some(Int_ty), Some(Int_ty) -> Some(Int_ty)
                     | _, _ -> exit(3))
  |_,_-> None;;


let rec type_check_exprs (exprs: exp_node list)(f: fun_frame option) = 
  match exprs, f with
  | exp::rest, f -> ignore(type_check_expr (exp)(f)); type_check_exprs(rest)(f)
  | [], _ -> assert(true);;


let type_check_stmt (stmt: stmt_node)(f: fun_frame option) = 
  match stmt, f with
  | Return(exp), f -> (match (type_check_expr exp f), f with
                    | Some(String_ty), None -> exit(3) (* Cannot return string type from top level, only int is allowed *)
                    | Some(Int_ty), None -> assert(true)
                    | Some(t1), Some(FunFrame(FunDec(_,t2,_,_,_))) -> if t1 = t2 then assert(true) else exit(3)
                    | _, _ -> assert(false))
  | _, _ -> assert(false);;

let rec type_check_stmts (stmts: stmt_node list)(f: fun_frame option): unit = 
  match stmts, f with
  | stmt::rest, f -> ignore(type_check_stmt(stmt)(f)); type_check_stmts(rest)(f)
  | [], _ -> assert(true);;

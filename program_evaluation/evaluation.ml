open Ast.Ast_nodes
open Ast.Context
open Tables.Function_table
open Stack
open Stack_frame

let rec eval_expr (e: exp_node) (s: stack) (f: functiontab): stack = 
  match e, seektop s with
  | Int(v), _ -> push s (ExpResult(Int(v)))
  | Add(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> v1+v2)
  | Sub(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> v1-v2)
  | Mul(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> v1*v2)
  | Div(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> v1/v2)
  | Rem(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> (v1 - (v1/v2)*v2))
  | BinOr(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> v1 lor v2)
  | BinAnd(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> v1 land v2)
  | Xor(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> v1 lxor v2)
  | Or(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> if(v1!=0||v2!=0) then 1 else 0)
  | And(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> if(v1!=0&&v2!=0) then 1 else 0)
  | Leq(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> if(v1 <= v2) then 1 else 0)
  | Geq(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> if(v1 >= v2) then 1 else 0)
  | Less(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> if(v1 < v2) then 1 else 0)
  | Greater(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> if(v1 > v2) then 1 else 0)
  | Neq(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> if(v1 != v2) then 1 else 0)
  | Eq(e1, e2), _ ->  eval_binop_expr e1 e2 s f (fun v1 v2 -> if(v1 = v2) then 1 else 0)
  | Not(e), _ -> let s1 = eval_expr e s f in 
                (
                  match seektop s1 with 
                  | Some(ExpResult(Int(v))) 
                  | Some(Frame(_,_, Some(Int(v)))) -> push (pop s1) (ExpResult(Int(if v!= 0 then 0 else 1)))
                  | _ -> exit(3)
                )
  | _ ,_-> assert(false)
and eval_binop_expr (e1: exp_node) (e2: exp_node) (s: stack) (f: functiontab) (op: int->int->int): stack = 
  let s1 = eval_expr e1 s f in 
    (
      match seektop s1 with
      | Some(ExpResult(Int(v1))) 
      | Some(Frame(_,_, Some(Int(v1)))) -> (let s2 = eval_expr e2 (pop s1) f in (
                                    match seektop s2 with
                                    | Some(ExpResult(Int(v2))) -> push (pop s2) (ExpResult(Int(op v1 v2)))
                                    | Some(Frame(_,_, Some(Int(v2)))) -> push (pop s2) (ExpResult(Int(op v1 v2)))
                                    | _ -> exit(3)
                                    ))
      | _ -> exit(3)
    )

let rec eval_stmts (stmts: stmt_node list) (s: stack) (f: functiontab) = 
  match stmts with
  | stmt::rest -> eval_stmts rest (eval_stmt stmt s f) f
  | [] -> s
and eval_stmt (stmt: stmt_node) (s: stack) (ft: functiontab): stack = 
  match stmt,seektop s with
  | Return(e), Some(Frame(Glob_ct, _, _)) -> (let v = (match seektop (eval_expr e s ft) with
                                                      | Some(ExpResult(Int(v)))-> v
                                                      | _ -> exit(3)) 
                                              in (
                                                  print_newline();
                                                  exit(v)
                                                )
                                              )
  | FunCallStmt("exit", [e]), _ -> (let v = (match seektop (eval_expr e s ft) with
                                                      | Some(ExpResult(Int(v)))-> v
                                                      | _ -> exit(3)) 
                                              in (
                                                  print_newline();
                                                  exit(v)
                                                )
                                              )

  | FunCallStmt("printint", [e]), Some(Frame(Glob_ct, _,_)) ->  (let v = (match seektop (eval_expr e s ft) with
                                                      | Some(ExpResult(Int(v)))-> v
                                                      | _ -> exit(3)) 
                                              in print_int v
                                              );s
  | _,_ -> assert(false)


let [@warning "-5"] eval (p: program) (f: functiontab): unit= 
  match p with
  | Program(_,_,stmts) -> ignore(eval_stmts stmts (Stack([Frame(Glob_ct, empty_vartab, None)])) f)

open Ast.Ast_nodes
open Ast.Context
open Global_table

let rec eval_expr (e: exp_node) (globals: globtab) (c: context): albatross_type = 
  match e, c with
  | Ident(x), Glob_ct -> (match lookup_globtab globals x with
                          | Some(vt) -> vt 
                          | None -> exit(3))
  | Ident(_), Func_ct(_) -> assert(false)
  | Add(e1, e2), c -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                    | Int(v1), Int(v2) -> Int(v1+v2)
                    | _,_ -> exit(3))
  | Sub(e1, e2), c  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                    | Int(v1), Int(v2) -> Int(v1-v2)
                    | _,_ -> exit(3))
  | Mul(e1, e2), c  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                    | Int(v1), Int(v2) -> Int(v1*v2)
                    | _,_ -> exit(3))
  | Div(e1, e2), c  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                    | Int(v1), Int(v2) -> Int(v1/v2)
                    | _,_ -> exit(3))
  | Rem(e1, e2), c  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                      | Int(v1), Int(v2) -> Int(v1 - ((v1/v2)*v2))
                      | _,_ -> exit(3))
  | BinOr(e1, e2), c  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                        | Int(v1), Int(v2) -> Int(v1 lor v2)
                        | _,_ -> exit(3))
  | BinAnd(e1, e2), c  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                        | Int(v1), Int(v2) -> Int(v1 land v2)
                        | _,_ -> exit(3))
  | Xor(e1, e2), _  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                        | Int(v1), Int(v2) -> Int(v1 lxor v2)
                        | _,_ -> exit(3))
  | Or(e1, e2), c  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                  | Int(v1), Int(v2) -> Int( if (v1 != 0 || v2 != 0) then 1 else 0)
                  | _,_ -> exit(3))
  | And(e1, e2), c  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                  | Int(v1), Int(v2) -> Int( if (v1 != 0 && v2 != 0) then 1 else 0)
                  | _,_ -> exit(3))
  | Leq(e1, e2), c  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                  | Int(v1), Int(v2) -> Int( if (v1 <= v2) then 1 else 0)
                  | _,_ -> exit(3))
  | Geq(e1, e2), c  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                  | Int(v1), Int(v2) -> Int( if (v1 >= v2) then 1 else 0)
                  | _,_ -> exit(3))
  | Less(e1, e2), c  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                  | Int(v1), Int(v2) -> Int( if (v1 < v2) then 1 else 0)
                  | _,_ -> exit(3))
  | Greater(e1, e2), c  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                  | Int(v1), Int(v2) -> Int( if (v1 > v2) then 1 else 0)
                  | _,_ -> exit(3))
  | Neq(e1, e2), c  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                  | Int(v1), Int(v2) -> Int( if (v1 != v2) then 1 else 0)
                  | _,_ -> exit(3))
  | Eq(e1, e2), c  -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                  | Int(v1), Int(v2) -> Int( if (v1 = v2) then 1 else 0)
                  | _,_ -> exit(3))
  | Not(e), _  -> (match eval_expr e globals c with
                  | Int(v)-> Int( if (v!=0) then 0 else 1)
                  | _-> exit(3))
  | Int(v), _  -> Int(v)
  | String(v), _  -> String(v)
  | FunCallExp(_, _), _  -> assert(false)
  | Nil, _  -> Void;;

let rec init_global_vars (vars: vardec_node list) (globals: globtab): globtab = 
  match vars with 
  | VarDec(x, _, e)::rest -> init_global_vars rest (update_globtab globals x (eval_expr e globals Glob_ct))
  | [] -> globals

let init_global_funcs (_: fundec_node list) (_: globtab): globtab = empty_globtab

let rec eval_stmts (stmts: stmt_node list)(globals: globtab)(c: context): globtab =
  match stmts with
  | stmt::rest -> eval_stmts rest (eval_stmt stmt globals c) c
  | [] -> globals
and [@warning "-21"] eval_stmt (stmt: stmt_node)(globals: globtab)(c: context): globtab = 
  match stmt, c with
  | Return(e), Glob_ct -> print_newline() ;exit(match eval_expr e globals c with Int(v) -> v | Void -> 0| String(_) -> exit(3) | Function(_)-> exit(3)); globals
  | FunCallStmt(x,e::[]), c when x = "exit"-> print_newline() ;exit(match eval_expr e globals c with Int(v) -> v | Void -> 0| String(_) -> exit(3) | Function(_)-> exit(3)); globals
  | FunCallStmt(x, e::[]), c when x = "printint" -> print_int(match eval_expr e globals c with Int(v) -> v | Void -> 0| String(_) -> exit(3) | Function(_)-> exit(3)); globals
  | IfThenElse(e, thn, el), c -> (match eval_expr e globals c with 
                              | Int(v) -> if v!=0 then eval_stmts thn globals c else eval_stmts el globals c 
                              | _ -> exit(3))
  | Assign(x, e), Glob_ct-> (match eval_expr e globals c with
                            | vt -> update_globtab globals x vt)
  | Assign(_,_), Func_ct(_) -> assert(false) 
  | _, _ -> assert(false)

let [@warning "-5"] eval (p: program) = 
  match p with
  | Program(vars,_,stmts) -> ignore(eval_stmts stmts (init_global_vars vars empty_globtab) Glob_ct)

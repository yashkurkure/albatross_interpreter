open Ast.Ast_nodes
open Ast.Context
open Global_table

let rec eval_expr (e: exp_node) (globals: globtab) (c: context): albatross_type = 
  match e, c with
  | Ident(_), _ -> assert(false)
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
  | Not(_), _  -> assert(false)
  | Int(v), _  -> Int(v)
  | String(v), _  -> String(v)
  | FunCallExp(_, _), _  -> assert(false)
  | Nil, _  -> Void;;

(* let rec init_global_vars (vars: vardec_node list) (globals: globtab): globtab = 
  match vars with 
  | VarDec(x, _, e)::rest -> init_global_vars rest (update_globtab globals x (eval_expr e globals))
  | [] -> globals

let init_global_funcs (_: fundec_node list) (_: globtab): globtab = empty_globtab *)

let rec eval_stmts (stmts: stmt_node list)(globals: globtab)(c: context): unit =
  match stmts with
  | stmt::rest -> eval_stmt stmt globals c; eval_stmts rest globals c
  | [] -> print_string ""
and eval_stmt (stmt: stmt_node)(globals: globtab)(c: context): unit = 
  match stmt, c with
  | Return(e), Glob_ct -> print_newline() ;exit(match eval_expr e globals c with Int(v) -> v | Void -> 0| String(_) -> exit(3) | Function(_)-> exit(3))
  | FunCallStmt(x,e::[]), c when x = "exit"-> print_newline() ;exit(match eval_expr e globals c with Int(v) -> v | Void -> 0| String(_) -> exit(3) | Function(_)-> exit(3))
  | FunCallStmt(x, e::[]), c when x = "printint" -> print_int(match eval_expr e globals c with Int(v) -> v | Void -> 0| String(_) -> exit(3) | Function(_)-> exit(3))
  | _, _ -> assert(false)

let eval (p: program) = 
  match p with
  | Program(_,_,stmts) -> eval_stmts stmts empty_globtab Glob_ct

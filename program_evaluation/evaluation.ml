open Ast.Ast_nodes
open Ast.Context
open Global_table
 
let rec eval_expr (e: exp_node) (globals: globtab) (c: context): albatross_type = 
  match e, c with
  | Ident(_), _ -> assert(false)
  | Add(e1, e2), _ -> (match eval_expr e1 globals c, eval_expr e2 globals c with
                    | Int(v1), Int(v2) -> Int(v1+v2)
                    | _,_ -> exit(3))
  | Sub(_, _), _  -> assert(false)
  | Mul(_, _), _  -> assert(false)
  | Div(_, _), _  -> assert(false)
  | Rem(_, _), _  -> assert(false)
  | BinOr(_, _), _  -> assert(false)
  | BinAnd(_, _), _  -> assert(false)
  | Xor(_, _), _  -> assert(false)
  | Or(_, _), _  -> assert(false)
  | And(_, _), _  -> assert(false)
  | Leq(_, _), _  -> assert(false)
  | Geq(_, _), _  -> assert(false)
  | Less(_, _), _  -> assert(false)
  | Greater(_, _), _  -> assert(false)
  | Neq(_, _), _  -> assert(false)
  | Eq(_, _), _  -> assert(false)
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

(* File main.ml  *)

(*let rec print_string_list l=
  match l with
  | hd::tl -> print_string hd; print_string_list tl
  | [] -> print_newline() *)
  open Ast_nodes

  let print_exps (_exps: exp_node list)= ()

  let rec print_exp_node (exp_node: exp_node) =
    match exp_node with
    | Add(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | Sub(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | Mul(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | Div(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | Rem(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | BinOr(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | BinAnd(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | Xor(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | Or(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | And(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | Leq(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | Geq(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | Less(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | Greater(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | Neq(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | Eq(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
    | Not(exp) -> print_exp_node exp;
    | Ident(ident) -> Printf.printf "Variable read \"%s\" type %s\n" ident "N/A (fix me)"
    | FunCallExp(ident, _) -> Printf.printf "Function called \"%s\" returns %s\n" ident "N/A (fix me)"
    | Nil -> assert(false) (*TODO: Implement me*)
    | _ -> assert(true) (*Do nohting*)
  
  
  let rec print_stmt_node (stmt_node: stmt_node) =
    match stmt_node with
    | Return(exp) -> print_exp_node exp
    | IfThenElse(exp, then_stmts, else_stmts) -> print_exp_node exp; print_stmts then_stmts; print_stmts else_stmts 
    | WhileOtherwise(exp, while_stmts, otherwise_stmts) -> print_exp_node exp; print_stmts while_stmts; print_stmts otherwise_stmts
    | Repeat(exp, stmts) -> print_exp_node exp; print_stmts stmts
    | Assign(ident, _) -> Printf.printf "Variable written \"%s\" type %s\n" ident "N/A (fix me)"
    | FunCallStmt(ident, _) -> Printf.printf "Function called \"%s\" returns %s\n" ident "N/A (fix me)"
  
  and print_stmts (stmts: stmt_node list)=
    match stmts with
    | hd::rest -> print_stmt_node hd; print_stmts rest
    | [] -> assert(true)
  
  let print_ast (p : program) = 
    match p with
    | Program(_, _, stmts) -> print_stmts stmts (*TODO: Print vardec_nodes and fundec_nodes*)
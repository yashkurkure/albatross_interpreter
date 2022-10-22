(* File main.ml  *)

(*let rec print_string_list l=
  match l with
  | hd::tl -> print_string hd; print_string_list tl
  | [] -> print_newline() *)


let print_exps (_exps: Ast.exp_node list)= ()

let rec print_exp_node (exp_node: Ast.exp_node) =
  match exp_node with
  | Ast.Add(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.Sub(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.Mul(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.Div(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.Rem(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.BinOr(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.BinAnd(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.Xor(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.Or(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.And(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.Leq(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.Geq(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.Less(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.Greater(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.Neq(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.Eq(exp1, exp2) -> print_exp_node exp1; print_exp_node exp2;
  | Ast.Not(exp) -> print_exp_node exp;
  | Ast.Ident(_) -> assert(false) (*TODO: Implement me*)
  | Ast.FunCallExp(_,exps) -> print_exps exps;assert(false)(*TODO: Implement me*)
  | Ast.Nil -> assert(false) (*TODO: Implement me*)
  | _ -> assert(true) (*Do nohting*)


let rec print_stmt_node (stmt_node: Ast.stmt_node) =
  match stmt_node with
  | Ast.Return(exp) -> print_exp_node exp
  | Ast.IfThenElse(exp, then_stmts, else_stmts) -> print_exp_node exp; print_stmts then_stmts; print_stmts else_stmts 
  | Ast.WhileOtherwise(exp, while_stmts, otherwise_stmts) -> print_exp_node exp; print_stmts while_stmts; print_stmts otherwise_stmts
  | Ast.Repeat(exp, stmts) -> print_exp_node exp; print_stmts stmts
  | Ast.Assign(_, _) -> assert(false) (*TODO: Implement me*)
  | Ast.FunCallStmt(_, _) -> assert(false) (*TODO: Implement me*)

and print_stmts (stmts: Ast.stmt_node list)=
  match stmts with
  | hd::rest -> print_stmt_node hd; print_stmts rest
  | [] -> assert(true)

let print_ast (p : Ast.program) = 
  match p with
  | Ast.Program(_, _, stmts) -> print_stmts stmts (*TODO: Print vardec_nodes and fundec_nodes*)

let _ =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  let lexer  = Sedlexing.with_tokenizer Lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  let result = parser lexer in
    print_ast result; flush stdout

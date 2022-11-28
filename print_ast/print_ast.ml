(* File main.ml  *)

(*let rec print_string_list l=
  match l with
  | hd::tl -> print_string hd; print_string_list tl
  | [] -> print_newline() *)
  open Ast.Ast_nodes
  open Symbol_table

  let ty_node_to_str(t: ty_node): string = 
  match t with
  | Int_ty -> "int"
  | String_ty -> "string"
  | Void_ty -> "void";;


  let print_exps (_exps: exp_node list)= ();;

  let rec print_exp(exp_node: exp_node) (symbol_table: symtab)=
    match exp_node with
    | Add(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | Sub(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | Mul(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | Div(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | Rem(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | BinOr(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | BinAnd(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | Xor(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | Or(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | And(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | Leq(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | Geq(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | Less(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | Greater(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | Neq(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | Eq(exp1, exp2) -> print_exp exp1 symbol_table; print_exp exp2 symbol_table;
    | Not(exp) -> print_exp exp symbol_table;
    | Ident(ident) -> (match lookup_symtab symbol_table ident with
                      | Some(t) -> Printf.printf "Variable read \"%s\" type %s\n" ident (ty_node_to_str t)
                      | None -> assert(false))
    | FunCallExp(ident, _) -> Printf.printf "Function called \"%s\" returns %s\n" ident "N/A (fix me)"
    | Nil -> assert(false) (*TODO: Implement me*)
    | _ -> assert(true);; (*Do nohting*)
  
  
  let rec print_stmt(stmt_node: stmt_node) (symbol_table: symtab): unit =
    match stmt_node with
    | Return(exp) -> print_exp exp symbol_table
    | IfThenElse(exp, then_stmts, else_stmts) -> (print_exp exp symbol_table; 
                                                  print_stmts then_stmts symbol_table; 
                                                  print_stmts else_stmts symbol_table)

    | WhileOtherwise(exp, while_stmts, otherwise_stmts) -> print_exp exp symbol_table; print_stmts while_stmts symbol_table; print_stmts otherwise_stmts symbol_table
    | Repeat(exp, stmts) -> print_exp exp symbol_table; print_stmts stmts symbol_table
    | Assign(ident, _) -> (match lookup_symtab symbol_table ident with
                          | Some(t) -> Printf.printf "Variable written \"%s\" type %s\n" ident (ty_node_to_str t)
                          | None -> assert(false))
    | FunCallStmt(ident, _) -> Printf.printf "Function called \"%s\" returns %s\n" ident "N/A (fix me)"
  
  and print_stmts (stmts: stmt_node list) (symbol_table: symtab)=
    match stmts with
    | hd::rest -> print_stmt hd symbol_table; print_stmts rest symbol_table
    | [] -> assert(true);;
  
    
  let rec print_vardecs (vardecs: vardec_node list) (symbol_table: symtab) : unit =
    match vardecs with
    | VarDec(ident, t, _)::rest -> Printf.printf "Variable declared \"%s\" type %s\n" ident (ty_node_to_str t); print_vardecs rest symbol_table
    | [] -> assert(true)
  
  let print_ast (p : program) (symbol_table: symtab): unit = 
    match p with
    | Program(vardecs, _, stmts) -> print_vardecs vardecs symbol_table; print_stmts stmts symbol_table (*TODO: Print vardec_nodes and fundec_nodes*)
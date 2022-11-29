(* File main.ml  *)

(*let rec print_string_list l=
  match l with
  | hd::tl -> print_string hd; print_string_list tl
  | [] -> print_newline() *)
  open Ast.Ast_nodes
  open Ast.Context
  open Tables.Symbol_table
  open Tables.Function_table

  let ty_node_to_str(t: ty_node): string = 
  match t with
  | Int_ty -> "int"
  | String_ty -> "string"
  | Void_ty -> "void";;


  let print_exps (_exps: exp_node list)= ();;

  let rec print_exp(exp_node: exp_node) (symbol_table: symtab) (function_table: functiontab) (c: context)=
    match exp_node with
    | Add(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Sub(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Mul(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Div(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Rem(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | BinOr(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | BinAnd(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Xor(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Or(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | And(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Leq(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Geq(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Less(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Greater(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Neq(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Eq(exp1, exp2) -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Not(exp) -> print_exp exp symbol_table function_table c;
    | Ident(ident) -> (match lookup_symtab symbol_table ident with
                      | Some(t) -> Printf.printf "Variable read \"%s\" type %s\n" ident (ty_node_to_str t)
                      | None -> assert(false))
    | FunCallExp(ident, _) -> Printf.printf "Function called \"%s\" returns %s\n" ident "N/A (fix me)"
    | Nil -> assert(true) (*Empty expression does nothing*)
    | _ -> assert(true);; (*Do nohting*)
  
  
  let rec print_stmt(stmt_node: stmt_node) (symbol_table: symtab) (function_table : functiontab) (c: context): unit =
    match stmt_node with
    | Return(exp) -> print_exp exp symbol_table function_table c
    | IfThenElse(exp, then_stmts, else_stmts) -> (print_exp exp symbol_table function_table c; 
                                                  print_stmts then_stmts symbol_table function_table c; 
                                                  print_stmts else_stmts symbol_table function_table c)

    | WhileOtherwise(exp, while_stmts, otherwise_stmts) -> print_exp exp symbol_table function_table c;
                                                           print_stmts while_stmts symbol_table function_table c;
                                                           print_stmts otherwise_stmts symbol_table function_table c
    | Repeat(exp, stmts) -> print_exp exp symbol_table function_table c; print_stmts stmts symbol_table function_table c
    | Assign(ident, exp) -> print_exp exp symbol_table function_table c; (match lookup_symtab symbol_table ident with
                          | Some(t) -> Printf.printf "Variable written \"%s\" type %s\n" ident (ty_node_to_str t)
                          | None -> assert(false))
    | FunCallStmt(ident, _) -> Printf.printf "Function called \"%s\" returns %s\n" ident "N/A (fix me)"
  
  and print_stmts (stmts: stmt_node list) (symbol_table: symtab) (function_table :functiontab) (c: context)=
    match stmts with
    | hd::rest -> print_stmt hd symbol_table function_table c; print_stmts rest symbol_table function_table c
    | [] -> assert(true);;
  
    
  let rec print_vardecs (vardecs: vardec_node list) (symbol_table: symtab) (function_table: functiontab) (c: context) : unit =
    match vardecs, c with
    | VarDec(ident, t, exp)::rest, Glob_ct -> (
        Printf.printf "Variable declared \"%s\" type %s\n" ident (ty_node_to_str t);
        print_exp exp symbol_table function_table c;
        print_vardecs rest symbol_table function_table c)
    
    | VarDec(ident, t, exp)::rest, Func_ct(_) -> (
      Printf.printf "Local Variable \"%s\" type %s\n" ident (ty_node_to_str t);
      print_exp exp symbol_table function_table c;
      print_vardecs rest symbol_table function_table c)

    | [], _ -> assert(true)
  
  let rec print_fundecs (fundecs: fundec_node list) (symbol_table: symtab) (function_table: functiontab): unit = 
    match fundecs with
    | FunDec(ident, t, _, vars, stmts)::rest -> (
        Printf.printf "Function declared \"%s\" returns %s\n" ident (ty_node_to_str t);
        print_vardecs vars symbol_table function_table (Func_ct(ident));
        print_stmts stmts symbol_table function_table (Func_ct(ident)));
        print_fundecs rest symbol_table function_table
    | [] -> assert(true)



  let print_ast (p : program) (symbol_table: symtab) (function_table: functiontab): unit = 
    match p with
    | Program(vardecs, fundecs, stmts) -> print_vardecs vardecs symbol_table function_table Glob_ct; print_fundecs fundecs symbol_table function_table;print_stmts stmts symbol_table function_table Glob_ct(*TODO: Print vardec_nodes and fundec_nodes*)
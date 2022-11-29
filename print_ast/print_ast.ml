(* File main.ml  *)

(*let rec print_string_list l=
  match l with
  | hd::tl -> print_string hd; print_string_list tl
  | [] -> print_newline() *)
  open Ast.Ast_nodes
  open Ast.Context
  open Tables.Symbol_table
  open Tables.Function_table
  open Util

  let ty_node_to_str(t: ty_node): string = 
  match t with
  | Int_ty -> "int"
  | String_ty -> "string"
  | Void_ty -> "void";;


  let rec print_exps (exps: exp_node list) (symbol_table: symtab) (function_table: functiontab) (c: context)= 
    match exps with
    | exp::rest -> print_exp exp symbol_table function_table c; print_exps rest symbol_table function_table c
    | [] -> assert(true)

  and print_exp(exp_node: exp_node) (symbol_table: symtab) (function_table: functiontab) (c: context): unit=
    match exp_node, c  with
    | Add(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Sub(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Mul(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Div(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Rem(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | BinOr(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | BinAnd(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Xor(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Or(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | And(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Leq(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Geq(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Less(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Greater(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Neq(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Eq(exp1, exp2), c -> print_exp exp1 symbol_table function_table c; print_exp exp2 symbol_table function_table c;
    | Not(exp), c -> print_exp exp symbol_table function_table c;
    | Ident(ident), Glob_ct -> (match lookup_symtab symbol_table ident with
                      | Some(t) -> Printf.printf "Variable read \"%s\" type %s\n" ident (ty_node_to_str t)
                      | None -> assert(false))
    | Ident(ident), Func_ct(x) -> (match lookup_symtab symbol_table ident with
                      | Some(t) -> (match lookup_functiontab function_table x with
                                    | Some(FunDec(_,_,args, vars,_)) -> (
                                      match contains args ident (fun (arg: fundec_arg)(arg_name: string) -> match arg with FunDecArg(arg_name', _) -> if arg_name = arg_name' then true else false),
                                            contains vars ident (fun (arg: vardec_node)(var_name: string) -> match arg with VarDec(var_name', _,_) -> if var_name = var_name' then true else false) with
                                      | true, false -> Printf.printf "Argument/local read \"%s\" type %s\n" ident (ty_node_to_str t)
                                      | false, true -> Printf.printf "Argument/local read \"%s\" type %s\n" ident (ty_node_to_str t)
                                      | false, false -> Printf.printf "Variable read \"%s\" type %s\n" ident (ty_node_to_str t)
                                      | true, true -> assert(false) 
                                    )
                                    | None -> assert(false) )
                      | None -> assert(false))
    | FunCallExp(ident, arg_inits), c -> print_exps arg_inits symbol_table function_table c; (match lookup_functiontab function_table ident with
                                    | Some(FunDec(_,t,_,_,_))-> Printf.printf "Function called \"%s\" returns %s\n" ident (ty_node_to_str t)
                                    | None -> assert(false))
    | Nil, _ -> assert(true) (*Empty expression does nothing*)
    | _,_ -> assert(true);; (*Do nohting*)
  
  
  let rec print_stmt(stmt_node: stmt_node) (symbol_table: symtab) (function_table : functiontab) (c: context): unit =
    match stmt_node, c with
    | Return(exp), c -> print_exp exp symbol_table function_table c
    | IfThenElse(exp, then_stmts, else_stmts), c -> (print_exp exp symbol_table function_table c; 
                                                  print_stmts then_stmts symbol_table function_table c; 
                                                  print_stmts else_stmts symbol_table function_table c)

    | WhileOtherwise(exp, while_stmts, otherwise_stmts), c -> print_exp exp symbol_table function_table c;
                                                           print_stmts while_stmts symbol_table function_table c;
                                                           print_stmts otherwise_stmts symbol_table function_table c
    | Repeat(exp, stmts), c -> print_exp exp symbol_table function_table c; print_stmts stmts symbol_table function_table c
    | Assign(ident, exp), Glob_ct -> print_exp exp symbol_table function_table c; (match lookup_symtab symbol_table ident with
                          | Some(t) -> Printf.printf "Variable written \"%s\" type %s\n" ident (ty_node_to_str t)
                          | None -> assert(false))
    | Assign(ident, exp), Func_ct(x) -> print_exp exp symbol_table function_table c; 
                          (match lookup_symtab symbol_table ident with
                      | Some(t) -> (match lookup_functiontab function_table x with
                                    | Some(FunDec(_,_,args, vars,_)) -> (
                                      match contains args ident (fun (arg: fundec_arg)(arg_name: string) -> match arg with FunDecArg(arg_name', _) -> if arg_name = arg_name' then true else false),
                                            contains vars ident (fun (arg: vardec_node)(var_name: string) -> match arg with VarDec(var_name', _,_) -> if var_name = var_name' then true else false) with
                                      | true, false -> Printf.printf "Argument/local written \"%s\" type %s\n" ident (ty_node_to_str t)
                                      | false, true -> Printf.printf "Argument/local written \"%s\" type %s\n" ident (ty_node_to_str t)
                                      | false, false -> Printf.printf "Variable written \"%s\" type %s\n" ident (ty_node_to_str t)
                                      | true, true -> assert(false) 
                                    )
                                    | None -> assert(false) )
                      | None -> assert(false))
    | FunCallStmt(ident, arg_inits), c ->  print_exps arg_inits symbol_table function_table c; (match lookup_functiontab function_table ident with
                                      | Some(FunDec(_,t,_,_,_))-> Printf.printf "Function called \"%s\" returns %s\n" ident (ty_node_to_str t)
                                      | None -> assert(false))
  
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
      Printf.printf "\tLocal variable \"%s\" type %s\n" ident (ty_node_to_str t);
      print_exp exp symbol_table function_table c;
      print_vardecs rest symbol_table function_table c)

    | [], _ -> assert(true)
  

  let rec add_function_args_symtab (funargs: fundec_arg list) (symbol_table: symtab): symtab = 
    match funargs with
    | FunDecArg(x,t)::rest -> add_function_args_symtab (rest) (update_symtab symbol_table x t)
    | [] -> symbol_table;;
  
  let rec add_function_locs_symtab (vardecs: vardec_node list) (symbol_table: symtab): symtab = 
    match vardecs with
    | VarDec(x, t, _)::rest -> add_function_locs_symtab (rest) (update_symtab symbol_table x t)
    | [] -> symbol_table;;

  let rec print_fundecs (fundecs: fundec_node list) (symbol_table: symtab) (function_table: functiontab): unit = 
    match fundecs with
    | FunDec(ident, t, args, vars, stmts)::rest -> (
        Printf.printf "Function declared \"%s\" returns %s\n" ident (ty_node_to_str t);
        print_fundec_args args symbol_table function_table;
        (
          let args_sym_table = add_function_args_symtab args symbol_table in (
            let args_locs_sym_table = add_function_locs_symtab vars args_sym_table in (
              print_vardecs vars args_locs_sym_table function_table (Func_ct(ident));
              print_stmts stmts args_locs_sym_table function_table (Func_ct(ident)));
            )
          )
        );
        print_fundecs rest symbol_table function_table
    | [] -> assert(true)
  and print_fundec_args (fundec_args: fundec_arg list) (symbol_table: symtab) (function_table: functiontab): unit = 
    match fundec_args with
    | FunDecArg(x, t):: rest -> Printf.printf "\tArgument \"%s\" type %s\n" x (ty_node_to_str t); print_fundec_args rest symbol_table function_table
    | [] -> assert(true)


  let print_ast (p : program) (symbol_table: symtab) (function_table: functiontab): unit = 
    match p with
    | Program(vardecs, fundecs, stmts) -> print_vardecs vardecs symbol_table function_table Glob_ct; print_fundecs fundecs symbol_table function_table;print_stmts stmts symbol_table function_table Glob_ct(*TODO: Print vardec_nodes and fundec_nodes*)
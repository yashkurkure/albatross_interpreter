open Ast.Ast_nodes
open Ast.Context
open Tables.Function_table
open Stack
open Stack_frame

(* Evaluate an expression. *)
let rec eval_expr (e: exp_node) (s: stack) (ft: functiontab): stack = 
  match e, seektop s with

  (* Push the int constant on the stack to be consumption. *)
  | Int(v), _ -> push s (ExpResult(Int(v)))

   (* Push string constant on the stack for consumption.  *)
  | String(v), _ -> push s (ExpResult(String(v)))

  (* Push the value of the identifier in the global variable table to the stack for comsumption. *)
  | Ident(x), Some(Frame(Glob_ct, vt, _)) -> push s (ExpResult(match lookup_vartab vt x with Some(v) -> v | None -> assert(false)))

  (* Push the value of the identifier from the function frame's variable table to the stack for consumption. *)
  | Ident(x), Some(Frame(Func_ct(_), vt, _)) -> (match lookup_vartab vt x with
                                                | Some(v) -> push s (ExpResult(v))
                                                | None -> (match look_glob_vartab s x with Some(v) -> push s (ExpResult(v)) | None -> assert(false)))
  (* Evaluate the operators. *)
  | Add(e1, e2), _ ->  eval_binop_expr e1 e2 s ft (fun v1 v2 -> v1+v2)
  | Sub(e1, e2), _ ->  eval_binop_expr e1 e2 s ft (fun v1 v2 -> v1-v2)
  | Mul(e1, e2), _ ->  eval_binop_expr e1 e2 s ft (fun v1 v2 -> v1*v2)
  | Div(e1, e2), _ ->  eval_binop_expr e1 e2 s ft (fun v1 v2 -> v1/v2)
  | Rem(e1, e2), _ ->  eval_binop_expr e1 e2 s ft (fun v1 v2 -> (v1 - (v1/v2)*v2))
  | BinOr(e1, e2), _ ->  eval_binop_expr e1 e2 s ft (fun v1 v2 -> v1 lor v2)
  | BinAnd(e1, e2), _ ->  eval_binop_expr e1 e2 s ft (fun v1 v2 -> v1 land v2)
  | Xor(e1, e2), _ ->  eval_binop_expr e1 e2 s ft (fun v1 v2 -> v1 lxor v2)
  | Or(e1, e2), _ ->  let s1 = eval_expr e1 s ft in 
                      (
                        match seektop s1 with
                        | Some(ExpResult(Int(v1))) 
                        | Some(Frame(_,_, Some(Int(v1)))) when v1 == 0 -> (let s2 = eval_expr e2 (pop s1) ft in (
                                                      match seektop s2 with
                                                      | Some(ExpResult(Int(v2))) -> push (pop s2) (ExpResult(Int(if v2==0 then 0 else 1)))
                                                      | Some(Frame(_,_, Some(Int(v2)))) -> push (pop s2) (ExpResult(Int(if v2==0 then 0 else 1)))
                                                      | _ -> assert(false)
                                                      ))
                        | Some(ExpResult(Int(v1))) 
                        | Some(Frame(_,_, Some(Int(v1)))) when v1 != 0 -> push (pop s1) (ExpResult(Int(if v1==0 then 0 else 1)))
                        | _ -> assert(false)
                      )
  | And(e1, e2), _ ->  let s1 = eval_expr e1 s ft in 
                      (
                        match seektop s1 with
                        | Some(ExpResult(Int(v1))) 
                        | Some(Frame(_,_, Some(Int(v1)))) when v1!=0 -> (let s2 = eval_expr e2 (pop s1) ft in (
                                                      match seektop s2 with
                                                      | Some(ExpResult(Int(v2))) -> push (pop s2) (ExpResult(Int(if v2!=0 then 1 else 0)))
                                                      | Some(Frame(_,_, Some(Int(v2)))) -> push (pop s2) (ExpResult(Int(if v2!=0 then 1 else 0)))
                                                      | _ -> assert(false)
                                                      ))
                        | Some(ExpResult(Int(v1))) 
                        | Some(Frame(_,_, Some(Int(v1)))) when v1==0 -> push (pop s1) (ExpResult(Int(0)))
                        | _ -> assert(false)
                      )
  | Leq(e1, e2), _ ->  eval_binop_expr e1 e2 s ft (fun v1 v2 -> if(v1 <= v2) then 1 else 0)
  | Geq(e1, e2), _ ->  eval_binop_expr e1 e2 s ft (fun v1 v2 -> if(v1 >= v2) then 1 else 0)
  | Less(e1, e2), _ ->  eval_binop_expr e1 e2 s ft (fun v1 v2 -> if(v1 < v2) then 1 else 0)
  | Greater(e1, e2), _ ->  eval_binop_expr e1 e2 s ft (fun v1 v2 -> if(v1 > v2) then 1 else 0)
  | Neq(e1, e2), _ ->  eval_binop_expr e1 e2 s ft (fun v1 v2 -> if(v1 != v2) then 1 else 0)
  | Eq(e1, e2), _ ->  eval_binop_expr e1 e2 s ft (fun v1 v2 -> if(v1 = v2) then 1 else 0)
  | Not(e), _ -> let s1 = eval_expr e s ft in 
                (
                  match seektop s1 with 
                  | Some(ExpResult(Int(v))) 
                  | Some(Frame(_,_, Some(Int(v)))) -> push (pop s1) (ExpResult(Int(if v!= 0 then 0 else 1)))
                  | _ -> assert(false)
                )
  (* Function calls*)
  | FunCallExp(x, args_init), _ ->  (* Look up the function in the function table to get its declaration. *)
                                    (match lookup_functiontab ft x with
                                    | Some(FunDec(_,_,arg_names,locs,stmts)) -> (eval_stmts 
                                                                                  
                                                                                  (* function body to be evaluated *)
                                                                                  (stmts)

                                                                                  (* the stack after initializing locals and variables and pushing the frame. *)
                                                                                  (
                                                                                    (* add and initialize the variables on the frame*)
                                                                                    add_function_locs_vartab
                                                                                    locs
                                                                                    (
                                                                                      (* add and initialize the arguemnts on the frame*)
                                                                                      add_function_args_vartab
                                                                                      arg_names
                                                                                      args_init
                                                                                      (
                                                                                        (* Push any empty frame *)
                                                                                        push s (Frame(Func_ct(x), empty_vartab, None))
                                                                                      ) 
                                                                                      ft
                                                                                    )
                                                                                    ft
                                                                                  )

                                                                                  (*function table*)
                                                                                  (ft)
                                                                                )
                                    | None -> assert(false) 
                                   )
  | Nil, _ ->  push s (ExpResult(Void))
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
                                    | _ -> assert(false)
                                    ))
      | _ -> assert(false)
    )

(* Evaluate a list of statements. *)
and eval_stmts (stmts: stmt_node list) (s: stack) (ft: functiontab): stack = 
  match stmts with
  | stmt::rest -> let s1 = eval_stmt stmt s ft in (match seektop s1 with
                                                  (* Stop execution if the function body returns, the return statement sets the result on the frame. *)
                                                  | Some(Frame(Func_ct(_),_,Some(_))) -> s1
                                                  (* If frame is in global context or the function has not returned, continue executing next statement. *)
                                                  | Some(Frame(_,_, None)) -> eval_stmts rest s1 ft
                                                  | _ -> assert(false)
                                                  )
  | [] -> s

(* Evalaute a statement. *)
and eval_stmt (stmt: stmt_node) (s: stack) (ft: functiontab): stack = 
  match stmt,seektop s with
  (* Return from global context. *)
  | Return(e), Some(Frame(Glob_ct, _, _)) ->
                                            (* Evalute the expression*)
                                            (let v = (match seektop (eval_expr e s ft) with
                                                      | Some(ExpResult(Int(v)))
                                                      | Some(Frame(_,_, Some(Int(v))))-> v
                                                      | Some(ExpResult(Void)) -> 0
                                                      | _ -> assert(false)) 
                                              in (
                                                  print_newline();
                                                  (*Returning from global is equivalent to exiting. *)
                                                  exit(v)
                                                )
                                              )
  (* Return from a function context. *)
  | Return(e), Some(Frame(Func_ct(x),vt,_)) -> (*Evaluate the expression and consume it right away. *)
                                              (let v = (match seektop (eval_expr e s ft) with
                                                      | Some(ExpResult(v))
                                                      | Some(Frame(_,_, Some(v)))-> v
                                                      | _ -> assert(false)) 
                                              in (
                                                  (* Set the functions result in the frame. *)
                                                  push (pop s) (Frame(Func_ct(x), vt, Some(v)))
                                                )
                                              )
  (* Intrinsic call to exit function. *)
  | FunCallStmt("exit", [e]), _ -> (let v = (match seektop (eval_expr e s ft) with
                                                      | Some(ExpResult(Int(v)))-> v
                                                      | _ -> assert(false)) 
                                              in (
                                                  print_newline();
                                                  exit(v)
                                                )
                                              )

  (* Intrinsic call to printint function.*)
  | FunCallStmt("printint", [e]), Some(Frame(_, _,_)) ->  let s1 = eval_expr e s ft in (let v = (match seektop s1 with
                                                      | Some(ExpResult(Int(v)))
                                                      | Some(Frame(_,_, Some(Int(v))))-> v
                                                      | _ -> assert(false)) 
                                              in Printf.printf "%d" v
                                              );pop s1

  (* Intrinsic call to printstring function. *)
  | FunCallStmt("printstring", [e]), Some(Frame(_, _,_)) ->  let s1 = eval_expr e s ft in (let v = (match seektop s1 with
                                                      | Some(ExpResult(String(v)))
                                                      | Some(Frame(_,_, Some(String(v)))) -> v
                                                      | _ -> assert(false)) 
                                              in Printf.printf "%s" v
                                              );pop s1

  | IfThenElse(e, thn, el), _ -> let s1 = eval_expr e s ft in 
                                 (
                                  match seektop s1 with
                                  | Some(ExpResult(Int(v))) 
                                  | Some(Frame(_,_, Some(Int(v)))) -> if (v!=0) then eval_stmts thn (pop s1) ft else eval_stmts el (pop s1) ft
                                  | _ -> assert(false)
                                 )

  | WhileOtherwise(e, b, o), _ -> let s1 = eval_expr e s ft in 
                                 (
                                  match seektop s1 with
                                  | Some(ExpResult(Int(v))) 
                                  | Some(Frame(_,_, Some(Int(v)))) -> if (v!=0) 
                                                                      then eval_stmt (WhileOtherwise(e,b,[])) (eval_stmts b (pop s1) ft) ft 
                                                                      else eval_stmts o (pop s1) ft
                                  | _ -> assert(false)
                                 )

  | Assign(x, e), Some(Frame(Glob_ct, vt, res)) -> let s1 = eval_expr e s ft in 
                                 (
                                  match seektop s1 with
                                  | Some(ExpResult(v)) 
                                  | Some(Frame(_,_, Some(v))) -> push (pop (pop s1)) (Frame(Glob_ct, update_vartab vt x v, res))
                                  | _ -> assert(false)
                                 )

  | Assign(x,e), Some(Frame(Func_ct(f), vt, res)) -> let s1 = eval_expr e s ft in
                                                  (
                                                    match seektop s1 with
                                                    | Some(ExpResult(v)) 
                                                    | Some(Frame(_,_, Some(v))) -> (match lookup_vartab vt x with 
                                                                                    | Some(_) -> push (pop (pop s1)) (Frame(Func_ct(f), update_vartab vt x v, res))
                                                                                    | None -> update_glob_vartab (pop s1) x v
                                                                                    )
                                                    | _ -> assert(false)
                                                  )

  | FunCallStmt(x, args_init), _ -> let s1 = (match lookup_functiontab ft x with
                                    | Some(FunDec(_,_,arg_names,locs,stmts)) -> (eval_stmts 
                                                                                  (stmts)
                                                                                  (
                                                                                    add_function_locs_vartab
                                                                                    locs
                                                                                    (
                                                                                      add_function_args_vartab
                                                                                      arg_names
                                                                                      args_init
                                                                                      (push s (Frame(Func_ct(x), empty_vartab, None)))
                                                                                      ft
                                                                                    )
                                                                                    ft
                                                                                  )
                                                                                  (ft)
                                                                                )
                                    | None -> assert(false) 
                                   ) in pop s1

  | Repeat(e, stmts), _ -> let s1 = eval_expr e s ft in (
                            match seektop s1 with
                            | Some(ExpResult(Int(v))) 
                            | Some(Frame(_,_, Some(Int(v)))) -> if (v!=0) 
                                                                then eval_stmt (Repeat(Int(v-1),stmts)) (eval_stmts stmts (pop s1) ft) ft 
                                                                else pop s1
                            | _ -> assert(false)
                          )
  | _,_ -> assert(false)

and add_function_args_vartab (args_names: fundec_arg list) (args_inits: exp_node list)(s:stack)(ft: functiontab): stack = 
  match args_names, args_inits with
  | FunDecArg(x,_)::rest, e::erest -> let s1 = eval_expr e (pop s) ft in (
                                  match seektop s1 with
                                  | Some(ExpResult(v)) 
                                  | Some(Frame(_,_, Some(v))) -> (match seektop s with
                                                                 | Some(Frame(c,vt,res)) -> add_function_args_vartab rest erest (push (pop s1) (Frame(c, update_vartab vt x v, res))) ft
                                                                 | _ -> assert(false)) 
                                  | _ -> assert(false)
                                 )
  | [],[] -> s
  | _, _ -> assert(false)

and add_function_locs_vartab (vardecs: vardec_node list) (s:stack)(ft: functiontab): stack = 
  match vardecs with
  | VarDec(x, _, e)::rest -> let s1 = eval_expr e s ft in (
                                  match seektop s1 with
                                  | Some(ExpResult(v)) 
                                  | Some(Frame(_,_, Some(v))) -> (match seektop (pop s1) with
                                                                | Some(Frame(c,vt,res)) -> add_function_locs_vartab rest (push (pop s1) (Frame(c, update_vartab vt x v, res))) ft
                                                                | _ -> assert(false))
                                  | _ -> assert(false)
                                 )
  | [] -> s;;


let rec init_glob_vars (vars: vardec_node list) (s: stack): stack = 
  match vars with
  | VarDec(x,_,e)::rest -> (match seektop (eval_expr e s empty_functiontab) with
                            | Some(ExpResult(v)) -> init_glob_vars rest (match s with 
                                                                          | Stack([Frame(c,vt,res)]) -> Stack([Frame(c, update_vartab vt x v, res)])
                                                                          | _ -> assert(false))
                            | _ -> assert(false))
  | _ -> s

let [@warning "-5"] eval (p: program) (f: functiontab): unit= 
  match p with
  | Program(vars,_,stmts) -> ignore(eval_stmts stmts (init_glob_vars vars (Stack([Frame(Glob_ct, empty_vartab, None)]))) f)

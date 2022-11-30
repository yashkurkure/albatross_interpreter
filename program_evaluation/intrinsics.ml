open Ast.Ast_nodes

let register_intrinsics (p: program): program = 
  match p with
  | Program(vars, funs, stmts) -> Program(
                                          vars, 
                                          (
                                            FunDec("exit", Void_ty, [FunDecArg("x", Int_ty)],[], [Return(Nil)])
                                            ::
                                            FunDec("printint", Void_ty, [FunDecArg("x", Int_ty)], [], [Return(Nil)])
                                            ::
                                            funs
                                          ), 
                                          stmts
                                        )
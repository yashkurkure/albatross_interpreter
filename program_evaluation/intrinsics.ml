open Ast.Ast_nodes

(* Register the intrinsics*)
let register_intrinsics (p: program): program = 
  match p with
  | Program(vars, funs, stmts) -> Program(
                                          vars, 
                                          (
                                            FunDec("exit", Void_ty, [FunDecArg("_x_", Int_ty)],[], [Return(Nil)])
                                            ::
                                            FunDec("printint", Void_ty, [FunDecArg("_x_", Int_ty)], [], [Return(Nil)])
                                            ::
                                            FunDec("printstring", Void_ty, [FunDecArg("_x_", String_ty)], [], [Return(Nil)])
                                            ::
                                            funs
                                          ), 
                                          stmts
                                        )
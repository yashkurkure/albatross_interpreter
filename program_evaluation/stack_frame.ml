open Ast.Context

(* Defines the elements of the stack. *)

(* The runtime types of expressions and symbols. *)
type albatross_type = Int of int | String of string | Void

(* A variable table for each stack frame. *)
and vartab = string -> albatross_type option

let empty_vartab = fun _ -> None

(* Look up in the var table*)
let lookup_vartab (vartable: vartab) (x: string): albatross_type option = vartable x

(* Update/Add variable to the var table. *)
let update_vartab (vartable: vartab) (x: string) (v: albatross_type) = fun y -> if y = x then Some(v) else vartable y

(* Remove a variable from the var table. *)
let remove_vartab (vartable: vartab) (x: string) = fun y -> if y = x then None else vartable y

(* Defines the frame of the stack. *)
(* Frame: used for functions, which contain the context, varirable table for the function and the runtime return value. *)
(* ExpResult: containts the result of an evaluated expression. *)
type frame = Frame of context*vartab*albatross_type option | ExpResult of albatross_type
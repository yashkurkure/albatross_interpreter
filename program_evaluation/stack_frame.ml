open Ast.Context

type albatross_type = Int of int | String of string | Void
and vartab = string -> albatross_type option

let empty_vartab = fun _ -> None

let lookup_globtab (vartable: vartab) (x: string): albatross_type option = vartable x

let update_vartab (vartable: vartab) (x: string) (v: albatross_type) = fun y -> if y = x then Some(v) else vartable y

let remove_vartab (vartable: vartab) (x: string) = fun y -> if y = x then None else vartable y

type frame = Frame of context*vartab*albatross_type option | ExpResult of albatross_type
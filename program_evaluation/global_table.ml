type albatross_type = Int of int | String of string | Void | Function of albatross_type

type globtab = string -> albatross_type option

let empty_globtab = fun _ -> None

let lookup_globtab (global_table: globtab) (x: string): albatross_type option = global_table x

let update_globtab (symbol_table: globtab) (x: string) (v: albatross_type) = fun y -> if y = x then Some(v) else symbol_table y
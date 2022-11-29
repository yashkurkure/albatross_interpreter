type exp_node=
  Add     of exp_node * exp_node
| Sub     of exp_node * exp_node
| Mul     of exp_node * exp_node
| Div     of exp_node * exp_node
| Rem     of exp_node * exp_node
| BinOr   of exp_node * exp_node
| BinAnd  of exp_node * exp_node
| Xor     of exp_node * exp_node
| Or      of exp_node * exp_node
| And     of exp_node * exp_node
| Leq     of exp_node * exp_node
| Geq     of exp_node * exp_node
| Less    of exp_node * exp_node
| Greater of exp_node * exp_node
| Neq     of exp_node *exp_node
| Eq      of exp_node * exp_node
| Not     of exp_node
| Int     of int
| String  of string
| Ident   of string
| FunCallExp of string*(exp_node list)
| Nil

type stmt_node = 
| Return          of exp_node
| IfThenElse      of exp_node * (stmt_node list) * (stmt_node list)
| WhileOtherwise  of exp_node * (stmt_node list) * (stmt_node list)
| Repeat          of exp_node * (stmt_node list)
| Assign          of string * exp_node
| FunCallStmt         of string * (exp_node list)

type ty_node = Int_ty | String_ty | Void_ty

type vardec_node = VarDec of string * ty_node * exp_node

type fundec_arg = FunDecArg of string * ty_node

type fundec_node = FunDec of string * ty_node * (fundec_arg list) * (vardec_node list) * (stmt_node list)

type program = Program of (vardec_node list) * (fundec_node list) * (stmt_node list)

let rec contains_vardec (l: vardec_node list) (x: string): bool = 
  match l with
  | VarDec(ident, _, _)::rest -> if ident = x then true else contains_vardec rest x
  | [] -> false;;
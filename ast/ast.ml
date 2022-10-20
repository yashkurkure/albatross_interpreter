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
| Ident   of string
| FunCall of string*(exp_node list)
| Nil

type stmt_node = 
| Return          of exp_node
| IfThenElse      of exp_node * (stmt_node list) * (stmt_node list)
| WhileOtherwise  of exp_node * (stmt_node list) * (stmt_node list)
| Repeat          of exp_node * (stmt_node list)
| Assign          of string * exp_node
| FunCall         of string * (exp_node list)

type ty_node = Int_ty | String_ty | Void_ty

type vardec_node = VarDec of string * ty_node * exp_node

type fun_param = FunParam of string * ty_node

type fundec_node = FunDec of string * ty_node * (fun_param list) * (vardec_node list) * (stmt_node list)

type program = Program of (string list) * (string list) * (string list)
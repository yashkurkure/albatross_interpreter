/* File parser.mly (unchanged) */
%{
    (*let surroundParens s:string = "("^s^")"

    (* let surroundBrackets s:string = "["^s^"]" *)

    let surroundQuotes s:string = "\""^s^"\""

    let concat (s1:string) (s2:string) = s1^s2

    let newline s: string = s^"\n"

    (*let rec concats (l: string list): string = 
        match l with
        | hd::tl -> print_string hd; hd ^ (concats tl)
        | [] -> ""*)

    (* let append l1 l2 =
        let rec loop acc l1 l2 =
            (match l1, l2 with
            | [], [] -> List.rev acc
            | [], h :: t -> loop (h :: acc) [] t
            | h :: t, l -> loop (h :: acc) t l)
        in
        loop [] l1 l2 *)
    
    *)


%}

%token <string> INT_CONSTANT
%token <string> NAME
%token <string> STRING_CONSTANT

%token RETURN
%token WHILE
%token OTHERWISE
%token IF
%token ELSE
%token REPEAT
%token VAR
%token FUN
%token EQUALITY
%token NOT_EQUALS
%token LESS_THAN
%token GREATER_THAN
%token GREATER_THAN_EQUAL
%token LESS_THAN_EQUAL
%token AND
%token OR
%token NOT
%token XOR
%token BINARY_AND
%token BINARY_OR
%token ADD
%token REMAINDER
%token SUB
%token MULTIPLY
%token DIVISION
%token TYPE_INT
%token TYPE_STRING
%token TYPE_VOID
%token TYPE_CHAR
%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token LEFT_CURLY
%token RIGHT_CURLY
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token SEMICOLON
%token ASSIGNMENT
%token COMMA
%token EOF

%start program             /* the entry point */
%type <Ast.program> program
%type <Ast.stmt_node list> stmts
%type <Ast.stmt_node> stmt
%type <Ast.vardec_node list> vardecls
%type <Ast.vardec_node> vardecl
%type <Ast.ty_node> albatrosstype
%type <Ast.fundec_node> fundecl
%type <Ast.fundec_arg> fundeclarg
%type <Ast.fundec_arg list> fundeclargs1
%type <Ast.fundec_arg list> fundeclargs2
%type <Ast.stmt_node> ifstmt
%type <Ast.stmt_node> returnstmt
%type <Ast.stmt_node> whilestmt
%type <Ast.stmt_node> repeatstmt
%type <Ast.stmt_node> assignstmt
%type <Ast.stmt_node> funcallstmt
// %type <string> arrayassignstmt
%type <Ast.stmt_node list> block
%type <Ast.stmt_node list> elsestmt
%type <Ast.stmt_node list> otherwisestmt
%type <Ast.exp_node> exp
%type <Ast.exp_node>  exp1
%type <Ast.exp_node>  exp2
%type <Ast.exp_node>  exp3
%type <Ast.exp_node>  exp4
%type <Ast.exp_node>  exp5
%type <Ast.exp_node>  exp6
%type <Ast.exp_node>  exp7
%type <Ast.exp_node>  exp8
%type <Ast.exp_node>  exp9
%type <Ast.exp_node>  exp10
%type <Ast.exp_node>  exp11
%type <Ast.exp_node> funcallexp
%type <Ast.exp_node list> funcallargs
%type <Ast.exp_node list> funcallargs2
// %type <string> arrayread
%%


program:
vardecls=list(vardecl); fundecls=list(fundecl); stmts=list(stmt); EOF { Ast.Program(vardecls, fundecls, stmts)}

albatrosstype:
TYPE_INT        {Ast.Int_ty}
| TYPE_STRING   {Ast.String_ty}
| TYPE_VOID     {Ast.Void_ty}

fundecl:
FUN NAME albatrosstype LEFT_PARENTHESIS RIGHT_PARENTHESIS LEFT_CURLY vardecls stmts RIGHT_CURLY                  { Ast.FunDec($2, $3, [], $7, $8) }
| FUN NAME albatrosstype LEFT_PARENTHESIS fundeclargs1 RIGHT_PARENTHESIS LEFT_CURLY vardecls stmts RIGHT_CURLY   {  Ast.FunDec($2, $3, $5, $8, $9) }

funblock:
LEFT_CURLY vardecls stmts RIGHT_CURLY   { $2 ^ $3 }

fundeclargs1:
fundeclarg fundeclargs2 { $1::$2 }
| fundeclarg            { [$1] }

fundeclargs2:
COMMA fundeclargs1  { $2 }

fundeclarg:
NAME albatrosstype  { Ast.FunDecArg($1, $2) }


vardecls: 
vardecl vardecls    { $1::$2 }
|                   { [] }

vardecl:
VAR NAME albatrosstype ASSIGNMENT exp SEMICOLON { Ast.VarDec($2, $3, $5) }

stmts: 
stmt stmts  { $1::$2 }
|           { [] }

stmt:
 returnstmt     { $1 }
| ifstmt        { $1 }
| whilestmt     { $1 }
| repeatstmt    { $1 }
| assignstmt    { $1 }
| funcallstmt   { $1 }


funcallstmt:
NAME LEFT_PARENTHESIS funcallargs RIGHT_PARENTHESIS SEMICOLON { Ast.FunCallStmt($1, $3) }

ifstmt:
IF LEFT_PARENTHESIS exp RIGHT_PARENTHESIS block elsestmt    { Ast.IfThenElse($3, $5, $6) }

elsestmt:
ELSE block  { $2 }
|           { [] }

whilestmt:
WHILE LEFT_PARENTHESIS exp RIGHT_PARENTHESIS block otherwisestmt    { Ast.WhileOtherwise($3, $5, $6) }

otherwisestmt:
OTHERWISE block { $2 }
|               { [] }

repeatstmt:
REPEAT LEFT_PARENTHESIS exp RIGHT_PARENTHESIS block { Ast.Repeat($3, $5) }

returnstmt: 
RETURN exp SEMICOLON    { Ast.Return($2) }
| RETURN SEMICOLON      { Ast.Return(Ast.Nil) }

assignstmt:
NAME ASSIGNMENT exp SEMICOLON   { Ast.Assign($1, $3) }

block:
LEFT_CURLY stmts RIGHT_CURLY    { $2 }

exp: exp1   { $1 }

exp1:
exp1 OR exp2    { Ast.Or($1, $3) }
| exp2          { $1 }

exp2:
exp2 AND exp3   { Ast.And($1, $3) }
| exp3          { $1 }

exp3:
exp3 BINARY_OR exp4 { Ast.BinOr($1, $3) }
| exp4              { $1 }

exp4:
exp4 XOR exp5   { Ast.Xor($1, $3) }
| exp5          { $1 }

exp5:
exp5 BINARY_AND exp6    { Ast.BinAnd($1, $3) }
| exp6                  { $1 }

exp6:
exp6 EQUALITY exp7      { Ast.Eq($1, $3) }
| exp6 NOT_EQUALS exp7  { Ast.Neq($1, $3) }
| exp7                  { $1 }

exp7:
exp7 LESS_THAN exp8             { Ast.Less($1, $3) }
| exp7 GREATER_THAN exp8        { Ast.Greater($1, $3) }
| exp7 GREATER_THAN_EQUAL exp8  { Ast.Geq($1, $3) }
| exp7 LESS_THAN_EQUAL exp8     { Ast.Leq($1, $3) }
| exp8                          { $1}

exp8:
exp8 ADD exp9   { Ast.Add($1, $3) }
| exp8 SUB exp9 { Ast.Sub($1, $3) }
| exp9          { $1 }

exp9:
 exp9 REMAINDER exp10  { Ast.Rem($1, $3) }
| exp9 MULTIPLY exp10  { Ast.Mul($1, $3) }
| exp9 DIVISION exp10  { Ast.Div($1, $3) }
| exp10                { $1 }

exp10:
NOT exp10 { Ast.Not($2) }
| exp11   { $1}

exp11:
INT_CONSTANT                              { Ast.Int(int_of_string $1) }
| STRING_CONSTANT                         { Ast.String($1)}
| NAME                                    { Ast.Ident($1) }
| LEFT_PARENTHESIS exp RIGHT_PARENTHESIS  { $2 }
| funcallexp                                 { $1 }

funcallexp:
NAME LEFT_PARENTHESIS funcallargs RIGHT_PARENTHESIS { Ast.FunCallExp($1, $3) }

funcallargs:
exp funcallargs2        { $1::$2 }
|                       { [] }

funcallargs2:
COMMA exp funcallargs2  { $2::$3 }
|                       { [] }
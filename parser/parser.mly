(* A parser implementation using menhir *)

%{
    open Ast.Ast_nodes
%}

(* Tokens *)
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
//%token TYPE_CHAR - for char types support
%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token LEFT_CURLY
%token RIGHT_CURLY
//%token LEFT_BRACKET - for array support
//%token RIGHT_BRACKET - for array support
%token SEMICOLON
%token ASSIGNMENT
%token COMMA
%token EOF

%start program             /* the entry point */
%type <program> program
%type <stmt_node list> stmts
%type <stmt_node> stmt
%type <vardec_node list> vardecls
%type <vardec_node> vardecl
%type <ty_node> albatrosstype
%type < fundec_node> fundecl
%type < fundec_arg> fundeclarg
%type < fundec_arg list> fundeclargs1
%type < fundec_arg list> fundeclargs2
%type < stmt_node> ifstmt
%type < stmt_node> returnstmt
%type < stmt_node> whilestmt
%type < stmt_node> repeatstmt
%type < stmt_node> assignstmt
%type < stmt_node> funcallstmt
// %type <string> arrayassignstmt
%type < stmt_node list> block
%type < stmt_node list> elsestmt
%type < stmt_node list> otherwisestmt
%type < exp_node> exp
%type < exp_node>  exp1
%type < exp_node>  exp2
%type < exp_node>  exp3
%type < exp_node>  exp4
%type < exp_node>  exp5
%type < exp_node>  exp6
%type < exp_node>  exp7
%type < exp_node>  exp8
%type < exp_node>  exp9
%type < exp_node>  exp10
%type < exp_node>  exp11
%type < exp_node> funcallexp
%type < exp_node list> funcallargs
%type < exp_node list> funcallargs2
// %type <string> arrayread
%%


program:
vardecls=list(vardecl); fundecls=list(fundecl); stmts=list(stmt); EOF {  Program(vardecls, fundecls, stmts)}

albatrosstype:
TYPE_INT        { Int_ty}
| TYPE_STRING   { String_ty}
| TYPE_VOID     { Void_ty}

fundecl:
FUN NAME albatrosstype LEFT_PARENTHESIS RIGHT_PARENTHESIS LEFT_CURLY vardecls stmts RIGHT_CURLY                  {  FunDec($2, $3, [], $7, $8) }
| FUN NAME albatrosstype LEFT_PARENTHESIS fundeclargs1 RIGHT_PARENTHESIS LEFT_CURLY vardecls stmts RIGHT_CURLY   {   FunDec($2, $3, $5, $8, $9) }

fundeclargs1:
fundeclarg fundeclargs2 { $1::$2 }
| fundeclarg            { [$1] }

fundeclargs2:
COMMA fundeclargs1  { $2 }

fundeclarg:
NAME albatrosstype  {  FunDecArg($1, $2) }


vardecls: 
vardecl vardecls    { $1::$2 }
|                   { [] }

vardecl:
VAR NAME albatrosstype ASSIGNMENT exp SEMICOLON {  VarDec($2, $3, $5) }

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
NAME LEFT_PARENTHESIS funcallargs RIGHT_PARENTHESIS SEMICOLON {  FunCallStmt($1, $3) }

ifstmt:
IF LEFT_PARENTHESIS exp RIGHT_PARENTHESIS block elsestmt    {  IfThenElse($3, $5, $6) }

elsestmt:
ELSE block  { $2 }
|           { [] }

whilestmt:
WHILE LEFT_PARENTHESIS exp RIGHT_PARENTHESIS block otherwisestmt    {  WhileOtherwise($3, $5, $6) }

otherwisestmt:
OTHERWISE block { $2 }
|               { [] }

repeatstmt:
REPEAT LEFT_PARENTHESIS exp RIGHT_PARENTHESIS block {  Repeat($3, $5) }

returnstmt: 
RETURN exp SEMICOLON    {  Return($2) }
| RETURN SEMICOLON      {  Return(Nil) }

assignstmt:
NAME ASSIGNMENT exp SEMICOLON   {  Assign($1, $3) }

block:
LEFT_CURLY stmts RIGHT_CURLY    { $2 }

exp: exp1   { $1 }

exp1:
exp1 OR exp2    {  Or($1, $3) }
| exp2          { $1 }

exp2:
exp2 AND exp3   {  And($1, $3) }
| exp3          { $1 }

exp3:
exp3 BINARY_OR exp4 {  BinOr($1, $3) }
| exp4              { $1 }

exp4:
exp4 XOR exp5   {  Xor($1, $3) }
| exp5          { $1 }

exp5:
exp5 BINARY_AND exp6    {  BinAnd($1, $3) }
| exp6                  { $1 }

exp6:
exp6 EQUALITY exp7      {  Eq($1, $3) }
| exp6 NOT_EQUALS exp7  {  Neq($1, $3) }
| exp7                  { $1 }

exp7:
exp7 LESS_THAN exp8             {  Less($1, $3) }
| exp7 GREATER_THAN exp8        {  Greater($1, $3) }
| exp7 GREATER_THAN_EQUAL exp8  {  Geq($1, $3) }
| exp7 LESS_THAN_EQUAL exp8     {  Leq($1, $3) }
| exp8                          { $1}

exp8:
exp8 ADD exp9   {  Add($1, $3) }
| exp8 SUB exp9 {  Sub($1, $3) }
| exp9          { $1 }

exp9:
 exp9 REMAINDER exp10  {  Rem($1, $3) }
| exp9 MULTIPLY exp10  {  Mul($1, $3) }
| exp9 DIVISION exp10  {  Div($1, $3) }
| exp10                { $1 }

exp10:
NOT exp10 {  Not($2) }
| exp11   { $1}

exp11:
INT_CONSTANT                              {  Int(int_of_string $1) }
| STRING_CONSTANT                         {  String($1)}
| NAME                                    {  Ident($1) }
| LEFT_PARENTHESIS exp RIGHT_PARENTHESIS  { $2 }
| funcallexp                                 { $1 }

funcallexp:
NAME LEFT_PARENTHESIS funcallargs RIGHT_PARENTHESIS {  FunCallExp($1, $3) }

funcallargs:
exp funcallargs2        { $1::$2 }
|                       { [] }

funcallargs2:
COMMA exp funcallargs2  { $2::$3 }
|                       { [] }
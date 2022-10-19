/* File parser.mly (unchanged) */
%{
    let surroundParens s:string = "("^s^")"

    (* let surroundBrackets s:string = "["^s^"]" *)

    let surroundQuotes s:string = "\""^s^"\""

    let concat (s1:string) (s2:string) = s1^s2

    let newline s: string = s^"\n"

    (*let rec concats (l: string list): string = 
        match l with
        | hd::tl -> print_string hd; hd ^ (concats tl)
        | [] -> ""*)



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
%type <string> program
%type <string> stmts
%type <string> stmt
// %type <string> vardecls
%type <string> vardecl
%type <string> albatrosstype
// %type <string> fundecl
// %type <string> fundeclarg
// %type <string> fundeclargs1
// %type <string> fundeclargs2
%type <string> ifstmt
%type <string> returnstmt
%type <string> whilestmt
%type <string> repeatstmt
%type <string> assignstmt
// %type <string> arrayassignstmt
// %type <string> block
// %type <string> elsestmt
// %type <string> otherwisestmt
%type <string> exp
%type <string> exp1
%type <string> exp2
%type <string> exp3
%type <string> exp4
%type <string> exp5
%type <string> exp6
%type <string> exp7
%type <string> exp8
%type <string> exp9
%type <string> exp10
%type <string> exp11
%type <string> equality
%type <string> comparison
%type <string> addsub
%type <string> muldivrem
// %type <string> funcall
// %type <string> funcallstmt
// %type <string> funcallargs
// %type <string> funcallargs2
// %type <string> arrayread
%%

program:
vardecl         { $1 }
|stmt           { $1 }

// vardecls:
// vardecl vardecls {$1 ^ $2}
// |                {""}

albatrosstype:
TYPE_INT        {"int"}
| TYPE_STRING   {"string"}
| TYPE_VOID     {"void"}

vardecl:
VAR NAME albatrosstype ASSIGNMENT exp SEMICOLON  {newline $5}

stmts: 
stmt stmts      {$1 ^ $2}
|               {""}

stmt:
 returnstmt          {$1}
| ifstmt             {$1}
| whilestmt             {$1}
| repeatstmt             {$1}
| assignstmt             {$1}

ifstmt:
IF LEFT_PARENTHESIS exp RIGHT_PARENTHESIS LEFT_CURLY stmts RIGHT_CURLY {(newline $3) ^ $6}

whilestmt:
WHILE LEFT_PARENTHESIS exp RIGHT_PARENTHESIS LEFT_CURLY stmts RIGHT_CURLY {(newline $3) ^ $6}

repeatstmt:
REPEAT LEFT_PARENTHESIS exp RIGHT_PARENTHESIS LEFT_CURLY stmts RIGHT_CURLY {(newline $3) ^ $6}

returnstmt: 
RETURN exp SEMICOLON {(newline $2)}
| RETURN SEMICOLON {""}

assignstmt:
NAME ASSIGNMENT exp SEMICOLON {(newline $3)}

exp: exp1    {$1}

exp1:
exp1 OR exp2    { surroundParens (concat($1) (concat "||" $3))}
| exp2          { $1 }

exp2:
exp2 AND exp3    { surroundParens (concat($1) (concat "&&" $3))}
| exp3          {$1}

exp3:
exp3 BINARY_OR exp4    { surroundParens (concat($1) (concat "|" $3))}
| exp4          {$1}

exp4:
exp4 XOR exp5    { surroundParens (concat($1) (concat "^" $3))}
| exp5          {$1}

exp5:
exp5 BINARY_AND exp6    { surroundParens (concat($1) (concat "&" $3))}
| exp6          {$1}

exp6:
exp6 equality exp7    { surroundParens (concat($1) (concat $2 $3))}
| exp7          {$1}

equality:
EQUALITY  {"=="}
| NOT_EQUALS  {"<>"}

exp7:
exp7 comparison exp8    { surroundParens (concat($1) (concat $2 $3))}
| exp8          {$1}

comparison:
LESS_THAN  {"<"}
| GREATER_THAN  {">"}
| GREATER_THAN_EQUAL  {">="}
| LESS_THAN_EQUAL  {"<="}

exp8:
exp8 addsub exp9    {surroundParens (concat($1) (concat $2 $3))}
| exp9          {$1}

addsub:
ADD    {"+"}
| SUB {"-"}

exp9:
exp9 muldivrem exp10    {surroundParens (concat($1) (concat $2 $3))}
| exp10          {$1}

muldivrem:
REMAINDER {"%"}
| MULTIPLY  {"*"}
| DIVISION  {"/"}

exp10:
NOT exp10 {surroundParens (concat "!" $2)}
| exp11   { $1}

exp11:
INT_CONSTANT   { surroundParens $1 }
| STRING_CONSTANT { surroundParens ( surroundQuotes $1 )}
| NAME           { surroundParens $1 }
| LEFT_PARENTHESIS exp RIGHT_PARENTHESIS  { $2 }
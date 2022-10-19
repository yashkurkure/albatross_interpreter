/* File parser.mly (unchanged) */
%{
    let surroundParens s:string = "("^s^")"

    (* let surroundBrackets s:string = "["^s^"]" *)

    (*let surroundQuotes s:string = "\""^s^"\""*)

    (*let concat (s1:string) (s2:string) = s1^s2*)

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
%token TYPE
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
// %type <string> vardecl
// %type <string> fundecl
// %type <string> fundeclarg
// %type <string> fundeclargs1
// %type <string> fundeclargs2
%type <string> ifstmt
%type <string> returnstmt
%type <string> whilestmt
%type <string> repeatstmt
// %type <string> assignstmt
// %type <string> arrayassignstmt
// %type <string> block
// %type <string> elsestmt
// %type <string> otherwisestmt
%type <string> exp
// %type <string> exp1
// %type <string> exp2
// %type <string> exp3
// %type <string> exp4
// %type <string> exp5
// %type <string> exp6
// %type <string> exp7
// %type <string> exp8
// %type <string> exp9
// %type <string> exp10
// %type <string> exp11
// %type <string> equality
// %type <string> comparison
// %type <string> addsub
// %type <string> muldivrem
// %type <string> funcall
// %type <string> funcallstmt
// %type <string> funcallargs
// %type <string> funcallargs2
// %type <string> arrayread
%%

program:
stmt           {$1}

stmts: 
stmt stmts      {$1 ^ $2}
|               {""}

stmt:
 returnstmt          {$1}
| ifstmt             {$1}
| whilestmt             {$1} 
| repeatstmt             {$1} 

ifstmt:
IF LEFT_PARENTHESIS exp RIGHT_PARENTHESIS LEFT_CURLY stmts RIGHT_CURLY {(newline $3) ^ $6}

whilestmt:
WHILE LEFT_PARENTHESIS exp RIGHT_PARENTHESIS LEFT_CURLY stmts RIGHT_CURLY {(newline $3) ^ $6}

repeatstmt:
REPEAT LEFT_PARENTHESIS exp RIGHT_PARENTHESIS LEFT_CURLY stmts RIGHT_CURLY {(newline $3) ^ $6}

returnstmt: 
RETURN exp SEMICOLON {(newline $2)}
| RETURN SEMICOLON {""}

exp: 
INT_CONSTANT   { surroundParens $1 }
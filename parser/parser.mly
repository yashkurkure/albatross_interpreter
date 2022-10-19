/* File parser.mly (unchanged) */
%{
    let surroundParens s:string = "("^s^")"

    let surroundBrackets s:string = "["^s^"]"

    let surroundQuotes s:string = "\""^s^"\""

    let concat (s1:string) (s2:string) = s1^s2

    let newline s: string = s^"\n"


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
%token PLUS
%token REMAINDER
%token MINUS
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

%start program             /* the entry point */
%type <string> program
%%

program:
stmt                   {$1}
| vardecl              {$1}
| fundecl              {$1}
| funcall               {$1}


// vardecls:
// vardecl vardecls         { concat $1 $2 }
// | vardecl                { $1 }
// | fundecls               { $1 }

vardecl:
VAR NAME TYPE ASSIGNMENT exp SEMICOLON  { newline $5 }
|                                       { "" }

// fundecls:
// fundecl fundecls { concat $1 $2 }
// | fundecl {$1}
// | stmts {$1}

fundecl:
FUN NAME TYPE LEFT_PARENTHESIS RIGHT_PARENTHESIS block {$6}
| FUN NAME TYPE LEFT_PARENTHESIS fundeclargs1 RIGHT_PARENTHESIS block {$7}
| {""}

fundeclargs1:
fundeclarg fundeclargs2 {""}
| fundeclarg {""}

fundeclargs2:
COMMA fundeclargs1 {""}

fundeclarg:
NAME TYPE   {""}

// stmts:
// stmts2 stmt {concat $1 $2}

// stmts2:
// stmt  {$1}
// |stmts {$1}


stmt:
returnstmt                      { $1 }
| ifstmt                        { $1 }
| whilestmt                     { $1 }
| repeatstmt                    { $1 }
| assignstmt                    { $1 }
| arrayassignstmt               { $1 }
| funcallstmt                   { $1 }
|                               { "" }

arrayassignstmt:
NAME LEFT_BRACKET exp RIGHT_BRACKET ASSIGNMENT exp SEMICOLON {newline $6 }

assignstmt:
NAME ASSIGNMENT exp SEMICOLON { newline $3 }

returnstmt:
RETURN exp SEMICOLON    { newline $2 }
| RETURN SEMICOLON    { "" }

ifstmt:
IF LEFT_PARENTHESIS exp RIGHT_PARENTHESIS block elsestmt { concat (concat(newline $3) $5) ($6)}

elsestmt:
ELSE block {$2}
|          {""}

whilestmt:
WHILE LEFT_PARENTHESIS exp RIGHT_PARENTHESIS block otherwisestmt { concat (concat (newline $3) ($5)) ($6)}

otherwisestmt:
OTHERWISE block {$2}
|               {""}

repeatstmt:
REPEAT LEFT_PARENTHESIS exp RIGHT_PARENTHESIS block  { concat (newline $3) ($5) }

funcallstmt:
funcall SEMICOLON {$1}


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
PLUS    {"+"}
| MINUS {"-"}

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
|NAME           { surroundParens $1 }
| LEFT_PARENTHESIS exp RIGHT_PARENTHESIS  { $2 }
| funcall {$1}
| arrayread {$1}

funcall:
NAME LEFT_PARENTHESIS funcallargs RIGHT_PARENTHESIS { concat ($1) (surroundParens $3) }

funcallargs:
exp funcallargs2 { concat $1  $2 }
|   { "" }

funcallargs2:
COMMA exp funcallargs2 {concat "," (concat $2  $3)}
| { "" }

arrayread:
NAME LEFT_BRACKET exp RIGHT_BRACKET {concat  $1 (surroundBrackets $3)}

block:
LEFT_CURLY vardecl RIGHT_CURLY { $2 }

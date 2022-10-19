let digit = [%sedlex.regexp? '0'..'9']
let int_constant = [%sedlex.regexp? Plus digit]
let name = [%sedlex.regexp? "\"",Star ("\\",("n"|"t"|"\""|"\\")| Compl (Chars "\"\\\n")),"\""]

open Parser
exception Eof

let rec token buf =
  match%sedlex buf with
  | "return" -> RETURN
  | "while" -> WHILE
  | "otherwise" -> OTHERWISE
  | "if" -> IF
  | "else" -> ELSE
  | "repeat" -> REPEAT
  | "var" -> VAR
  | "fun" -> FUN
  | "==" -> EQUALITY
  | "<>" -> NOT_EQUALS
  | '<' -> LESS_THAN
  | '>' -> GREATER_THAN
  | ">=" -> GREATER_THAN_EQUAL
  | "<=" -> LESS_THAN_EQUAL
  | "&&" -> AND
  | "||" -> OR
  | '!' -> NOT
  | '^' -> XOR
  | '&' -> BINARY_AND
  | '|' -> BINARY_OR
  | '+' -> LESS_THAN
  | '%' -> LESS_THAN
  | '-' -> LESS_THAN
  | '*' -> LESS_THAN
  | '/' -> LESS_THAN
  | "int" -> TYPE
  | "string" -> TYPE
  | "char" -> TYPE
  | "void" -> TYPE
  | name -> NAME (Sedlexing.Latin1.lexeme buf)
  | '(' -> LEFT_PARENTHESIS
  | ')' -> RIGHT_PARENTHESIS
  | '{' -> LEFT_CURLY
  | '}' -> RIGHT_CURLY
  | '[' -> LEFT_BRACKET
  | ']' -> RIGHT_BRACKET
  | ';' -> SEMICOLON
  | ":=" -> ASSIGNMENT
  | ',' -> COMMA
  | '\n' -> token buf
  | Plus (Chars " \t") -> token buf
  | int_constant -> INT_CONSTANT (Sedlexing.Latin1.lexeme buf)
  | eof -> raise Eof
  | _ -> failwith "Unexpected character"


(* let rec token buf =
  match%sedlex buf with
  | Plus (Chars " \t") -> token buf
  | '\n' ->   EOL
  | number -> INT_CONSTANT (Sedlexing.Latin1.lexeme buf)
  | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> TIMES
  | '/' -> DIV
  | '(' -> LPAREN
  | ')' -> RPAREN
  | ';' -> SEMICOLON
  | "return" -> RETURN
  | eof -> raise Eof
  | _ -> failwith "Unexpected character" *)

let digit = [%sedlex.regexp? ('0'..'9')]
let alphabet = [%sedlex.regexp? 'a'..'z'|'A'..'Z']
let alphanum = [%sedlex.regexp? digit|alphabet]
let int_constant = [%sedlex.regexp? Plus digit]
let name = [%sedlex.regexp? alphabet, (Star alphanum)]

let debug: bool = false

let debug_print (s:string) =
  if debug then (print_string s; print_newline())

open Parser
exception Eof

let rec token buf =
  match%sedlex buf with
  | "return" -> debug_print "RETURN"; RETURN
  | "while" -> debug_print "WHILE";WHILE
  | "otherwise" -> debug_print "RETURN";OTHERWISE
  | "if" -> debug_print "IF";IF
  | "else" -> debug_print "RETURN";ELSE
  | "repeat" -> debug_print "REPEAT";REPEAT
  | "var" -> debug_print "RETURN";VAR
  | "fun" -> debug_print "RETURN";FUN
  | "==" -> debug_print "RETURN";EQUALITY
  | "<>" -> debug_print "RETURN";NOT_EQUALS
  | '<' -> debug_print "RETURN";LESS_THAN
  | '>' -> debug_print "RETURN";GREATER_THAN
  | ">=" -> debug_print "RETURN";GREATER_THAN_EQUAL
  | "<=" -> debug_print "RETURN";LESS_THAN_EQUAL
  | "&&" -> debug_print "RETURN";AND
  | "||" -> debug_print "RETURN";OR
  | '!' -> debug_print "RETURN";NOT
  | '^' -> debug_print "RETURN";XOR
  | '&' -> debug_print "RETURN";BINARY_AND
  | '|' -> debug_print "RETURN";BINARY_OR
  | '+' -> debug_print "RETURN";ADD
  | '%' -> debug_print "RETURN";REMAINDER
  | '-' -> debug_print "RETURN";SUB
  | '*' -> debug_print "RETURN";MULTIPLY
  | '/' -> debug_print "RETURN";DIVISION
  | "int" -> debug_print "RETURN";TYPE
  | "string" -> debug_print "RETURN";TYPE
  | "char" -> debug_print "RETURN";TYPE
  | "void" -> debug_print "RETURN";TYPE
  | name -> debug_print "NAME";NAME (Sedlexing.Latin1.lexeme buf)
  | int_constant -> debug_print "RETURN"; INT_CONSTANT (Sedlexing.Latin1.lexeme buf)
  | '(' -> debug_print "LPAREN";LEFT_PARENTHESIS
  | ')' -> debug_print "RPAREN";RIGHT_PARENTHESIS
  | '{' -> debug_print "LCURLY";LEFT_CURLY
  | '}' -> debug_print "RCURLY";RIGHT_CURLY
  | '[' -> debug_print "RETURN";LEFT_BRACKET
  | ']' -> debug_print "RETURN";RIGHT_BRACKET
  | ';' -> debug_print "SEMICOLON";SEMICOLON
  | ":=" -> debug_print "ASSIGNMENT";ASSIGNMENT
  | ',' -> debug_print "COMMA";COMMA
  | '\n' -> token buf
  | Plus (Chars " \t") -> token buf
  | eof -> raise Eof
  | _ -> debug_print (Sedlexing.Latin1.lexeme buf); failwith "Unexpected character"


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

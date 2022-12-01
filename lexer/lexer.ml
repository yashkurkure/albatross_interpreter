
(*utils*)
let debug: bool = false
let debug_print (s:string) =
  if debug then (print_string s; print_newline())


let explode (s: string): char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec implode (l: char list) : string = 
  match l with
  | hd::tl -> (String.make 1 hd) ^ (implode tl)
  | [] -> ""



let rec _unescape (l: char list): char list= 
  match l with
  | '\\'::'n'::rest -> '\n'::rest
  | '\\'::'t'::rest -> '\t'::rest
  | '\\'::'\"'::rest -> '\"'::rest
  | '\\'::'\\'::rest -> '\\'::rest
  | hd::tl -> hd::_unescape tl
  | [] -> []

let unescape (s:string) = 
  implode (_unescape (explode s))

let stripQuotes str = 
  match explode str with
  | '\"'::rest -> (match List.rev rest with
                  | '\"'::rrest -> implode (List.rev rrest)
                  | _ -> str)
  | _ ->  str
  

(* regexes *)
let digit = [%sedlex.regexp? ('0'..'9')]
let alphabet = [%sedlex.regexp? 'a'..'z'|'A'..'Z']
let alphanum = [%sedlex.regexp? digit|alphabet]
let int_constant = [%sedlex.regexp? Plus digit]

let string_illlegal = [%sedlex.regexp? Chars ("\"\\\n") ]
let string_legal = [%sedlex.regexp? Compl (string_illlegal)]
let string_value = [%sedlex.regexp? ("\\",("n"|"t"|"\""|"\\"))]
let string_constant = [%sedlex.regexp? "\"", Star (string_value | string_legal) ,"\""]

let name = [%sedlex.regexp? alphabet, (Star alphanum)]


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
  | "var" -> debug_print "VAR";VAR
  | "fun" -> debug_print "FUN";FUN
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
  | "int" -> debug_print "TYPE_INT";TYPE_INT
  | "string" -> debug_print "TYPE_STRING";TYPE_STRING
  | "char" -> debug_print "TYPE_CHAR"; failwith "albatross does not support char types"
  | "void" -> debug_print "TYPE_VOID";TYPE_VOID
  | name -> debug_print "NAME";NAME (Sedlexing.Latin1.lexeme buf)
  | int_constant -> debug_print "INT_CONSTANT"; INT_CONSTANT (Sedlexing.Latin1.lexeme buf)
  | string_constant -> debug_print "STRING_CONSTANT"; STRING_CONSTANT (unescape (stripQuotes (Sedlexing.Latin1.lexeme buf)))
  | '(' -> debug_print "LPAREN";LEFT_PARENTHESIS
  | ')' -> debug_print "RPAREN";RIGHT_PARENTHESIS
  | '{' -> debug_print "LCURLY";LEFT_CURLY
  | '}' -> debug_print "RCURLY";RIGHT_CURLY
  | '[' -> debug_print "RETURN";failwith "Invalid character: ["
  | ']' -> debug_print "RETURN";failwith "Invalid character: ]"
  | ';' -> debug_print "SEMICOLON";SEMICOLON
  | ":=" -> debug_print "ASSIGNMENT";ASSIGNMENT
  | ',' -> debug_print "COMMA";COMMA
  | '\n' -> token buf
  | Plus (Chars " \t") -> token buf
  | eof -> EOF
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

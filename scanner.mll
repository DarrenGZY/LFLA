{ open Parser }

let Exp = 'e'('+'|'-')?['0'-'9']+

rule token = parse 
    [' ' '\t' '\r' '\n' ] { token lexbuf }
(* constructor key words*)
| 'L'   { VSCONST }
(* several kinds of delimiters *)
| '{'   { LBRACE }
| '}'   { RBRACE }
| "[["	{ LLBRACK}
| "]]"  { RRBRACK}
| '['   { LBRACK }
| ']'   { RBRACK }
| '('   { LPAREN }
| ')'   { RPAREN }
| ';'   { SEMI  }
| ','   { COMMA }
| ':'   { COLON }
| '='   { ASSIGN }
| "<<"  {LIN}
| ">>"  {RIN}
(* logical operators *)
| "&&"  { AND }
| "||"  { OR }
(* additive operators *)
| '+'   { PLUS }
| '-'   { MINUS }
| "+."  { PLUS_DOT }
| "-."  { MINUS_DOT }
(* multiplicative operators *)
| '*'   { TIMES }
| '/'   { DIVIDE }
| "*."  { TIMES_DOT }
| "/."  { DIVIDE_DOT }
(* unary operator *)
| '\''  { TRANSPOSE }
| '@'   { BELONGS }
(* comparation operators *)
| '<'   { LT }
| "<="  { LEQ }
| '>'   { GT }
| ">="  { GEQ }
| "=="  { EQ }
| "!="  { NEQ }
(* type identifier *)
| "var"         { VAR }
| "vector"      { VECTOR }
| "vecspace"    { VECSPACE }
| "matrix"      { MATRIX }
| "inspace"     { INSPACE }
| "affspace"    { AFFSPACE }
(* control flow statements *)
| "while"   { WHILE }
| "for"     { FOR }
| "if"      { IF }
| "else"    { ELSE }
| "break"   { BREAK }
| "continue"{ CONTINUE }
| "return"  { RETURN }
(* function declaration *)
| "function"    { FUNCTION }
(* Literal and identifers *)
| ['0'-'9']+ | ('.'['0'-'9']+Exp? | ['0'-'9']+ ('.'['0'-'9']*Exp? | Exp))as num  { LITERAL(num) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { ID(id) }
| eof   { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }


{ 
    open Lexing
    open Parser 
    (* 
     * update line number in the context 
     * *)
    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            { pos with pos_bol = lexbuf.lex_curr_pos;
                       pos_lnum = pos.pos_lnum+1
            }
}

let Exp = 'e'('+'|'-')?['0'-'9']+

rule token = parse 
 [' ' '\t' ]            { token lexbuf }
| ['\r' '\n']| "\r\n"   { next_line lexbuf; token lexbuf }
| "###"                 { comment lexbuf }
| '#'                   { line_comment lexbuf }
(* constructor key words and built-in functions*)
| 'L'       { VSCONST }
| "dim"     { DIM }
| "size"    { SIZE }
| "basis"   { BASIS }
| "print"   { PRINT }
| "rank"    { RANK }
| "trace"   { TRACE }
| "image"   { IMAGE }
| "eigenValue" { EVALUE }
| "ceil"    { CEIL }
| "floor"   { FLOOR }
| "sqrt"    { SQRT }
| "solve"   { SOLVE }

(* several kinds of delimiters *)
| '{'   { LBRACE }
| '}'   { RBRACE }
| '['   { LBRACK }
| ']'   { RBRACK }
| '('   { LPAREN }
| ')'   { RPAREN }
| ';'   { SEMI  }
| ','   { COMMA }
| ':'   { COLON }
| '='   { ASSIGN }
| "[["  { LLBRACK }
| "]]"  { RRBRACK }
| "<<"  { LIN }
| ">>"  { RIN }
(* logical operators *)
| "&&"  { AND }
| "||"  { OR }
(* additive operators *)
| "+"   { PLUS }
| "-"   { MINUS }
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
| '&'   { ACTION }
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
| eof       { EOF }
| _ as c { raise (Failure("illegal character " ^ Char.escaped c)) }

and comment = parse
 "###"      { token lexbuf }
| _         { comment lexbuf }

and line_comment = parse
 ['\n' '\r']   { token lexbuf }
| _             { line_comment lexbuf }

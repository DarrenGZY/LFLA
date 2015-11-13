%{ open Ast %}

%token VSCONST
%token LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN COLON SEMI COMMA
%token AND OR  
%token PLUS MINUS PLUS_DOT MINUS_DOT 
%token TIMES DIVIDE TIMES_DOT DIVIDE_DOT 
%token ASSIGN
%token TRANSPOSE BELONGS
%token LT LEQ GT GEQ EQ NEQ 
%token VAR VECTOR VECSPACE MATRIX INSPACE AFFSPACE
%token WHILE FOR IF ELSE BREAK CONTINUE RETURN FUNCTION
%token <string> LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left LT LEQ GT GEQ EQ NEQ
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left TIMES DIVIDE TIMES_DOT DIVIDE_DOT
%left TRANSPOSE BELONGS


%start program
%type<Ast.program> program

%%

program:
    programs EOF { $1 }

programs :
    /* nothing */                   { [], [] }
    | programs funtion_declaration  { fst $1, ($2::snd $1) }
    | programs normal_declaration   { ($2::fst $1), snd $1 } 

funtion_declaration :
    FUNCTION ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE { 
        {   fname=$2; 
            params=$4; 
            locals =  $7; 
            body= $8 } }

formals_opt:
    /* nothing */ { [] }
    | formal_list    { List.rev $1 }
  
formal_list :
	|formal  { $1 }
    | formal_list COMMA  formal  { $2 :: $1}
	
formal :	
	primitive_type ID                  { Vardecl({vname = name; data_type = $1})}
  | primitive_type  ID LBRACK RBRACK   { Arraydecl({aname = name; data_type = $1})}
               
 
vdecl_list :
	/* nothing */ { [] }
	|vdecl_list vdecl  { $2 :: $1  }
	
vdecl :
	primitive_type varid  SEMI       { List.map( fun name -> Vardecl({vname = name; data_type = $1}) ) $2 }
    | primitive_type  arrayid SEMI   { List.map( fun name -> Arraydecl({aname = name; data_type = $1}))$2 }  
 
varid :
    ID  {[$1]}	
	| varid COMMA ID {$3 :: $1} 

arrayid :
    ID LBRACK RBRACK { [$1]}
	| arrayid COMMA  ID LBRACK RBRACK {$3 :: $1}	

primitive_type:
    VAR         { VaR }
    | VECTOR    { VeC }
    | VECSPACE  { VecspA }
    | MATRIX    { MaT }
    | INSPACE   { InspA }
    | AFFSPACE  { AffspA }	
	
stmt_list:
	 /* nothing */                               { [] }
	 function_statements                         {$1}
    	
   
function_statements : 
	statement 	{$1}
	| statement function_statements  {$1 :: $2}
	
statement :
    expression SEMI                 { Expr($1) }
    | RETURN expression SEMI        { Return($2) }
    | LBRACE statement_list RBRACE  { Block($2) }
    | IF expression LBRACE statement_list RBRACE %prec NOELSE   /* How to recognize NOELSE*/
                                    { If($2, $4, []) }
    | IF expression LBRACE statement_list RBRACE ELSE LBRACE statement_list RBRACE 
                                    { If($2, $4, $8) }
    | FOR VAR ID ASSIGN LITERAL COLON LITERAL LBRACE statement_list RBRACE 
                                    { For($5, $7,$9) } /* TODO: if var is needed here */
    | WHILE expression LBRACE statement_list RBRACE { While($2, $4) }



expression:
    LITERAL                         { Literal($1) }
    | ID                            { Id($1) }
	| expression PLUS_DOT   expression  { Binop($1, AddDot, $3) }
    | expression MINUS_DOT  expression  { Binop($1, SubDot, $3) }
    | expression TIMES_DOT  expression  { Binop($1, MultDot, $3) }
    | expression DIVIDE_DOT expression  { Binop($1, DivDot, $3) }
    | expression PLUS   expression  { Binop($1, Add, $3) }
    | expression MINUS  expression  { Binop($1, Sub, $3) }
    | expression TIMES  expression  { Binop($1, Mult, $3) }
    | expression DIVIDE expression  { Binop($1, Div, $3) }
    | expression EQ     expression  { Binop($1, Equal, $3) }
    | expression NEQ    expression  { Binop($1, Neq, $3) }
    | expression LT     expression  { Binop($1, Less, $3) }
    | expression LEQ    expression  { Binop($1, Leq, $3) }
    | expression GT     expression  { Binop($1, Greater, $3) }
    | expression GEQ    expression  { Binop($1, Geq, $3) }
    | ID ASSIGN expression          { Assign($1, $3) }
    | ID LPAREN arguments_opt RPAREN { Call($1, $3) } 
    | LPAREN expression RPAREN      { $2 }
	| expression TRANSPOSE     		{Transpose($1)}
	| LBRACK LBRACK expression COMMA expression RBRACK RBRACK    {Binop($3,LieBracket,$5)}
	| expression @ expression      {Binop($1,Belongs, $3)}
	| LT COMMA RT                  {Binop($1, InnerProduct, $3)} 
	| vector                       {$1} 
    | matrix                        {$1}
	| vector_space                   {$1} 
	| inner_space                    {$1}
    | aff_space                     {$1} 	
    | arrays                       {$1}  
   
vector :
	LBRACK expression_list RBRACK   {Vector($2))} 
	
expression_list :
	exprssion        {[$1]}
	expression COMMA expression_list  {$1 :: $3}
	
matrix : 
	LBRACK matrix_elements_list: RBRACK  {Matrix ($2)}
	
matrix_elements_list :	
	row_elements_list    {$1}
	row_elements_list matrix_elements_list {$1 :: $2}
/* should change the definition of matrix, the semicolon conflicts with the break of statement*/
row_elements_list :
    expression_list SEMI {$1}
	
vector_space :
	VSCONST LPAREN expression_list RPAREN  {VectorSpace ($3)}
 
inner_space :
    INSPACE LPAREN expression COMMA expression RPAREN    {InSpace($3,$5)} 	 
	
aff_space :
	AFFSPACE LPAREN expression COMMA expression RPAREN {AffSpace($3,$5)}	
		 
	
arrays :
    LBRACE expresion_list RBRACE {Arra($2)} 	
	
                          
   

 



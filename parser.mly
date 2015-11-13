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
	
	
function_statements : 
    /* nothing */                               { [], [] }
    | function_statements normal_declaration    { ($2::fst $1), snd $1 }
    | function_statements statement             { fst $1, ($2::snd $1) }

normal_declaration :
    normal_declaration_expression SEMI { $1 }

normal_declaration_expression :
    variable_declaration_expression { $1 }
    | array_declaration_expression  { $1 }

variable_declaration_expression :
    var_declaration_expression          { $1 }
    | vector_declaration_expression     { $1 }  
    | matrix_declaration_expression     { $1 }
    | vecspace_declaration_expression   { $1 }
    | inspace_declaration_expression    { $1 }
    | affspace_declaration_expression   { $1 }


var_declaration_expression :
    VAR ID                      { Vardecl({vname = $2; value = Notknown; data_type = Var }) }
    | VAR ID ASSIGN LITERAL     { Vardecl({vname = $2; value = VValue($4); data_type = Var }) }

vector_declaration_expression :
    VECTOR ID   { Vardecl({vname = $2; value = Notknown; data_type = Vector }) }
    | VECTOR ID ASSIGN LBRACK vector_elements_list_opt RBRACK 
                { Vardecl({vname = $2; value = VecValue($5); data_type = Vector }) }

vector_elements_list_opt :
    /* nothing */           { [] }
    | vector_elements_list  { $1 }

vector_elements_list :
    LITERAL                                 { [$1] }
    | vector_elements_list COMMA LITERAL    { $3::$1 }

matrix_declaration_expression :
    MATRIX ID   { Vardecl({vname = $2; value = Notknown; data_type = Matrix }) }
    | MATRIX ID ASSIGN LBRACK matrix_elements_list RBRACK 
                { Vardecl({vname = $2; value = MatValue($5); data_type = Matrix }) }

matrix_elements_list :
    SEMI { [] }
    | row_elements_list SEMI matrix_elements_list { $1::$3 }

row_elements_list :
    LITERAL {[$1]}
    | row_elements_list COMMA LITERAL { $3::$1 }

vecspace_declaration_expression :
    VECSPACE ID { Vardecl({vname = $2; value = Notknown; data_type = VecSpace }) }
    | VECSPACE ID ASSIGN VSCONST LPAREN vecspace_elements_list RPAREN 
                { Vardecl({vname = $2; value = VecSpValue($6); data_type = VecSpace }) }

vecspace_elements_list :
    ID {[$1]}
    | vecspace_elements_list COMMA ID { $3::$1 }

inspace_declaration_expression :
    INSPACE ID  { Vardecl({vname = $2; value = Notknown; data_type = InSpace }) }
    | INSPACE ID ASSIGN INSPACE LPAREN ID COMMA ID RPAREN 
                { Vardecl({vname = $2; value = InSpValue($6,$8); data_type = InSpace }) }

affspace_declaration_expression :
    AFFSPACE ID { Vardecl({vname = $2; value = Notknown; data_type = AffSpace }) }
    | AFFSPACE ID ASSIGN AFFSPACE LPAREN ID COMMA ID RPAREN 
                { Vardecl({vname = $2; value = AffSpValue($6, $8); data_type = AffSpace }) }

array_declaration_expression :
    VAR ID LBRACK LITERAL RBRACK 
            { Arraydecl({ aname = $2; elements = []; data_type = Var; length = 0}) }
    | VAR ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE 
            { Arraydecl({ aname = $2; elements = $8; data_type = Var; length = List.length $8})}
    | VECTOR ID LBRACK LITERAL RBRACK 
            { Arraydecl({ aname = $2; elements = []; data_type = Vector; length = 0})}
    | VECTOR ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE 
            { Arraydecl({ aname = $2; elements = $8; data_type = Vector; length = List.length $8})}
    | MATRIX ID LBRACK LITERAL RBRACK 
            { Arraydecl({ aname = $2; elements = []; data_type = Matrix; length = 0})}
    | MATRIX ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE 
            { Arraydecl({ aname = $2; elements = $8; data_type = Matrix; length = List.length $8})}
    | INSPACE ID LBRACK LITERAL RBRACK 
            { Arraydecl({ aname = $2; elements = []; data_type = InSpace; length = 0})}
    | INSPACE ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE 
            { Arraydecl({ aname = $2; elements = $8; data_type = InSpace; length = List.length $8})}
    | AFFSPACE ID LBRACK LITERAL RBRACK 
            { Arraydecl({ aname = $2; elements = []; data_type = AffSpace; length = 0})}
    | AFFSPACE ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE 
            { Arraydecl({ aname = $2; elements = $8; data_type = AffSpace; length = List.length $8})}
    | VECSPACE ID LBRACK LITERAL RBRACK 
            { Arraydecl({ aname = $2; elements = []; data_type = VecSpace; length = 0})}
    | VECSPACE ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE 
            { Arraydecl({ aname = $2; elements = $8; data_type = VecSpace; length = List.length $8})}

array_elements_list :
    ID                                  { [Id($1)] }
    | LITERAL                           { [Literal($1)] }
    | array_elements_list COMMA ID      { Id($3)::$1 }
    | array_elements_list COMMA LITERAL { Literal($3)::$1 }


statement :
    expression SEMI                 { Expr($1) }
    | RETURN expression SEMI        { Return($2) }
    | LBRACE statement_list RBRACE  { Block(List.rev $2) }
    | IF expression LBRACE statement_list RBRACE %prec NOELSE 
                                    { If($2, $4, []) }
    | IF expression LBRACE statement_list RBRACE ELSE LBRACE statement_list RBRACE 
                                    { If($2, $4, $8) }
    | FOR VAR ID ASSIGN LITERAL COLON LITERAL LBRACE statement_list RBRACE 
                                    { For(int_of_string($5), int_of_string($7),$9) } /* TODO: if var is needed here */
    | WHILE expression LBRACE statement_list RBRACE { While($2, $4) }

statement_list :
    /* nothing */ { [] }
    | statement_list statement { $2::$1 }

expression:
    LITERAL                         { Literal($1) }
    | ID                            { Id($1) }
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

arguments_opt:
    /* nothing */   { [] }
  | arguments_list  { List.rev $1 }

arguments_list:
    expression        { [$1] }       
  | arguments_list COMMA expression { $3::$1 } 




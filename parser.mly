%{  
    open Ast 
    open Lexing
    open Parsing 
%}

%token VSCONST PRINT DIM SIZE BASIS 
%token LBRACE RBRACE LBRACK RBRACK LLBRACK RRBRACK LIN RIN LPAREN RPAREN COLON SEMI COMMA
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
%left LBRACK RBRACK
%left LT LEQ GT GEQ EQ NEQ
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left TIMES DIVIDE TIMES_DOT DIVIDE_DOT
%left AND OR
%left TRANSPOSE BELONGS

%start program
%type<Ast.program> program

%%

program:
    programs EOF { List.rev $1 }

programs :
    /* nothing */                   { [] }
    | programs funtion_declaration  { Function($2)::$1 }
    | programs global_normal_declaration   { Variable($2)::$1 } 

funtion_declaration :
    FUNCTION ID LPAREN parameter_list_opt RPAREN LBRACE function_statements RBRACE { 
        {   fname=$2; 
            params=$4; 
            body= List.rev $7 } }

parameter_list_opt :
    /* nothing */       { [] }
    | parameter_list    { List.rev $1 }

parameter_list :
    primitive_type ID                                           
        { [ Lvardecl({vname = $2; value = Notknown; data_type = $1; pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum })] }
    | primitive_type LBRACK RBRACK ID                           
        { [ Larraydecl({ aname = $4; elements = []; data_type = $1; length = 0; pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum})] } 
    | parameter_list COMMA primitive_type ID                    
        { Lvardecl({vname = $4; value = Notknown; data_type = $3; pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum})::$1 }
    | parameter_list COMMA primitive_type ID LBRACK RBRACK      
        { Larraydecl({aname = $4; elements = []; data_type = $3; length = 0; pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum})::$1 }

function_statements : 
    /* nothing */                                       { [] }
    | function_statements local_normal_declaration      { Local($2)::$1 }
    | function_statements statement                     { Body($2)::$1 }

local_normal_declaration :
    local_normal_declaration_expression SEMI { $1 }

local_normal_declaration_expression :
    variable_declaration_expression { Lvardecl($1) }
    | array_declaration_expression  { Larraydecl($1) }

global_normal_declaration :
    global_normal_declaration_expression SEMI { $1 }

global_normal_declaration_expression :
    variable_declaration_expression { Gvardecl($1) }
    | array_declaration_expression  { Garraydecl($1) }

variable_declaration_expression :
    var_declaration_expression          { $1 }
    | vector_declaration_expression     { $1 }  
    | matrix_declaration_expression     { $1 }
    | vecspace_declaration_expression   { $1 }
    | inspace_declaration_expression    { $1 }
    | affspace_declaration_expression   { $1 }


var_declaration_expression :
    VAR ID                      
        { 
            {vname = $2; value = Notknown; data_type = Var; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum } 
        }
   /* | VAR ID ASSIGN LITERAL     { Vardecl({vname = $2; value = VValue($4); data_type = Var }) }  expression contains LITERAL   */ 
    | VAR ID ASSIGN expression  
        { 
            {vname = $2; value = Expression($4); data_type = Var; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum } 
        }

vector_declaration_expression :
    VECTOR ID   
        { 
            {vname = $2; value = Notknown; data_type = Vector; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum } 
        }
    | VECTOR ID ASSIGN LBRACK vector_elements_list_opt RBRACK 
        { 
            {vname = $2; value = VecValue($5); data_type = Vector; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum } 
        }
    | VECTOR ID ASSIGN expression
        { 
            {vname = $2; value = Expression($4); data_type = Vector; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum } 
        }

vector_elements_list_opt :
    /* nothing */           { [] }
    | vector_elements_list  { List.rev $1 }

vector_elements_list :
    LITERAL                                 { [$1] }
    | vector_elements_list COMMA LITERAL    { $3::$1 }

matrix_declaration_expression :
    MATRIX ID   
        { 
            {vname = $2; value = Notknown; data_type = Matrix; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum } 
        }
    | MATRIX ID ASSIGN LBRACK matrix_elements_list RBRACK 
        { 
            {vname = $2; value = MatValue($5); data_type = Matrix; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum } 
        }
    | MATRIX ID ASSIGN expression
        { 
            {vname = $2; value = Expression($4); data_type = Matrix; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum } 
        }

matrix_elements_list :
    /* nothing */  { [] }
    | row_elements_list SEMI matrix_elements_list { (List.rev $1)::$3 }

row_elements_list :
    LITERAL {[$1]}
    | row_elements_list COMMA LITERAL { $3::$1 }

vecspace_declaration_expression :
    VECSPACE ID 
        { 
            {vname = $2; value = Notknown; data_type = VecSpace; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum }
        }
    | VECSPACE ID ASSIGN VSCONST LPAREN vecspace_elements_list RPAREN 
        { 
            {vname = $2; value = VecSpValue(List.rev $6); data_type = VecSpace; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum } 
        }

vecspace_elements_list :
    ID {[$1]}
    | vecspace_elements_list COMMA ID { $3::$1 }

inspace_declaration_expression :
    INSPACE ID  
        { 
            {vname = $2; value = Notknown; data_type = InSpace; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum } 
        }
    | INSPACE ID ASSIGN INSPACE LPAREN ID COMMA ID RPAREN 
        { 
            {vname = $2; value = InSpValue($6,$8); data_type = InSpace; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum } 
        }

affspace_declaration_expression :
    AFFSPACE ID 
        { 
            {vname = $2; value = Notknown; data_type = AffSpace; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum } 
        }
    | AFFSPACE ID ASSIGN AFFSPACE LPAREN ID COMMA ID RPAREN 
        { 
            {vname = $2; value = AffSpValue($6, $8); data_type = AffSpace; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum } 
        }

array_declaration_expression :
    VAR ID LBRACK LITERAL RBRACK 
        { 
            { aname = $2; elements = []; data_type = Var; length = 0; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum} 
        }
    | VAR ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE 
        { 
            { aname = $2; elements = (List.rev $8); data_type = Var; length = List.length $8; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum}
        }
    | VECTOR ID LBRACK LITERAL RBRACK 
        { 
            { aname = $2; elements = []; data_type = Vector; length = 0; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum}
        }
    | VECTOR ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE 
        { 
            { aname = $2; elements = (List.rev $8); data_type = Vector; length = List.length $8; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum}
        }
    | MATRIX ID LBRACK LITERAL RBRACK 
        { 
            { aname = $2; elements = []; data_type = Matrix; length = 0; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum}
        }
    | MATRIX ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE 
        { 
            { aname = $2; elements = (List.rev $8); data_type = Matrix; length = List.length $8; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum}
        }
    | INSPACE ID LBRACK LITERAL RBRACK 
        { 
            { aname = $2; elements = []; data_type = InSpace; length = 0; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum}
        }
    | INSPACE ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE 
        { 
            { aname = $2; elements = (List.rev $8); data_type = InSpace; length = List.length $8; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum}
        }
    | AFFSPACE ID LBRACK LITERAL RBRACK 
        { 
            { aname = $2; elements = []; data_type = AffSpace; length = 0; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum}
        }
    | AFFSPACE ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE 
        { 
            { aname = $2; elements = (List.rev $8); data_type = AffSpace; length = List.length $8; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum}
        }
    | VECSPACE ID LBRACK LITERAL RBRACK 
        { 
            { aname = $2; elements = []; data_type = VecSpace; length = 0; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum}
        }
    | VECSPACE ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE 
        { 
            { aname = $2; elements = (List.rev $8); data_type = VecSpace; length = List.length $8; 
            pos = let pos_start = Parsing.symbol_start_pos () in pos_start.pos_lnum}
        }

array_elements_list :
    ID                                  { [Id(Nid($1))] }
    | LITERAL                           { [Literal($1)] }
    | array_elements_list COMMA ID      { Id(Nid($3))::$1 }
    | array_elements_list COMMA LITERAL { Literal($3)::$1 }


statement :
    expression SEMI                 { Expr($1) }
    | RETURN    expression SEMI     { Return($2) }
    | BREAK     expression SEMI     { Break }
    | CONTINUE  expression SEMI     { Continue }
    | LBRACE statement_list RBRACE  { Block(List.rev $2) }
    | IF expression LBRACE statement_list RBRACE %prec NOELSE 
                                    { If($2, $4, []) }
    | IF expression LBRACE statement_list RBRACE ELSE LBRACE statement_list RBRACE 
                                    { If($2, $4, $8) }
    | FOR VAR ID ASSIGN expression COLON expression LBRACE statement_list RBRACE 
                                    { For($3, $5, $7,List.rev($9)) } /* TODO: if var is needed here */
    | WHILE expression LBRACE statement_list RBRACE { While($2, $4) }

statement_list :
    /* nothing */ { [] }
    | statement_list statement { $2::$1 }

expression:
    LITERAL                             { Literal($1) }
    | element                           { Id($1) }
    | expression TRANSPOSE              { Transpose($1) }
    | expression PLUS       expression  { Binop($1, Add, $3) }
    | expression MINUS      expression  { Binop($1, Sub, $3) }
    | expression TIMES      expression  { Binop($1, Mult, $3) }
    | expression DIVIDE     expression  { Binop($1, Div, $3) }
    | expression EQ         expression  { Binop($1, Equal, $3) }
    | expression NEQ        expression  { Binop($1, Neq, $3) }
    | expression LT         expression  { Binop($1, Less, $3) }
    | expression LEQ        expression  { Binop($1, Leq, $3) }
    | expression GT         expression  { Binop($1, Greater, $3) }
    | expression GEQ        expression  { Binop($1, Geq, $3) }
    | expression AND        expression  { Binop($1, And, $3) }
    | expression OR         expression  { Binop($1, Or, $3) }
    | expression PLUS_DOT   expression  { Binop($1, Add_Dot, $3) }
    | expression MINUS_DOT  expression  { Binop($1, Sub_Dot, $3) }
    | expression TIMES_DOT  expression  { Binop($1, Mult_Dot, $3) }
    | expression DIVIDE_DOT expression  { Binop($1, Div_Dot, $3) }
    | expression BELONGS    expression  { Belongs($1, $3) }
    | ID    LIN expression  COMMA   expression  RIN     { Inpro($1, $3, $5) }
    | LLBRACK   expression  COMMA   expression  RRBRACK { LieBracket($2, $4) }   
    | ID    ASSIGN  expression          { Assign($1, $3) }
    | ID    ASSIGN  LBRACE array_elements_list RBRACE   { AssignArr($1, $4) } 
    | ID    LPAREN  arguments_opt RPAREN{ Call($1, $3) } 
    | LPAREN    expression  RPAREN      { $2 }
    | builtin   LPAREN  element  RPAREN      { Builtin($3, $1) }
    | PRINT     LPAREN  expression RPAREN    { Print($3) } 

builtin:
    DIM         { Dim }
    | SIZE      { Size }
    | VSCONST   { Vsconst }    /* some problem here, should be multiple elements */
    | BASIS     { Basis }

/* normal identifier and array identifier */
element:
    | ID                          { Nid($1) }
    | ID LBRACK LITERAL RBRACK    { Arrayid($1, $3) }
    | ID LBRACK ID      RBRACK  { Arrayid($1, $3) }

arguments_opt:
    /* nothing */   { [] }
  | arguments_list  { List.rev $1 }

arguments_list:
    expression        { [$1] }       
  | arguments_list COMMA expression { $3::$1 } 

primitive_type:
    VAR         { Var }
    | VECTOR    { Vector }
    | VECSPACE  { VecSpace }
    | MATRIX    { Matrix }
    | INSPACE   { InSpace }
    | AFFSPACE  { AffSpace }


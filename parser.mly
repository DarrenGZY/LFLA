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
    programs EOF { }

programs :
    /* nothing */   
    | programs funtion_declaration { }
    | programs normal_declaration { } 

funtion_declaration :
    FUNCTION ID LPAREN parameter_list_opt RPAREN LBRACE function_statements RBRACE {}

parameter_list_opt :
    /* nothing */   { }
    | parameter_list  { }

parameter_list :
    primitive_type ID  { }
    | primitive_type ID LBRACK RBRACK { } 
    | parameter_list COMMA primitive_type ID { }
    | parameter_list COMMA primitive_type ID LBRACK RBRACK { }

function_statements : 
    /* nothing */   { [] }
    | function_statements normal_declaration  { }
    | function_statements statement          { }

normal_declaration :
    normal_declaration_expression SEMI { }

normal_declaration_expression :
    variable_declaration_expression { }
    | array_declaration_expression { }

variable_declaration_expression :
    var_declaration_expression          { }
    | vector_declaration_expression     { }  
    | matrix_declaration_expression     { }
    | vecspace_declaration_expression   { }
    | inspace_declaration_expression    { }
    | affspace_declaration_expression   { }


var_declaration_expression :
    VAR ID {  }
    | VAR ID ASSIGN expression { }

vector_declaration_expression :
    VECTOR ID {  }
    | VECTOR ID ASSIGN LBRACK vector_elements_list_opt RBRACK {  }

vector_elements_list_opt :
    /* nothing */   { }
    | vector_elements_list { }

vector_elements_list :
    LITERAL { }
    | vector_elements_list COMMA LITERAL {  }

matrix_declaration_expression :
    MATRIX ID {  }
    | MATRIX ID ASSIGN LBRACK matrix_elements_list RBRACK {  }

matrix_elements_list :
    SEMI {  }
    | row_elements_list SEMI matrix_elements_list {  }

row_elements_list :
    LITERAL {}
    | row_elements_list COMMA LITERAL {  }

vecspace_declaration_expression :
    VECSPACE ID { }
    | VECSPACE ID ASSIGN VSCONST LPAREN vecspace_elements_list RPAREN { }

vecspace_elements_list :
    ID {}
    | vecspace_elements_list COMMA ID {  }

inspace_declaration_expression :
    INSPACE ID {  }
    | INSPACE ID ASSIGN INSPACE LPAREN ID COMMA ID RPAREN { }

affspace_declaration_expression :
    AFFSPACE ID {  }
    | AFFSPACE ID ASSIGN AFFSPACE LPAREN ID COMMA ID RPAREN { }

array_declaration_expression :
    VAR ID LBRACK LITERAL RBRACK {}
    | VAR ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE {}
    | VECTOR ID LBRACK LITERAL RBRACK {}
    | VECTOR ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE {}
    | MATRIX ID LBRACK LITERAL RBRACK {}
    | MATRIX ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE {}
    | INSPACE ID LBRACK LITERAL RBRACK {}
    | INSPACE ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE {}
    | AFFSPACE ID LBRACK LITERAL RBRACK {}
    | AFFSPACE ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE {}
    | VECSPACE ID LBRACK LITERAL RBRACK {}
    | VECSPACE ID LBRACK LITERAL RBRACK ASSIGN LBRACE array_elements_list RBRACE {}

array_elements_list :
    ID {}
    | LITERAL {}
    | array_elements_list COMMA ID {}
    | array_elements_list COMMA LITERAL {}


statement :
    expression SEMI {}
    | RETURN expression SEMI {}
    | LBRACE statement_list RBRACE {}
    | IF expression LBRACE statement_list RBRACE %prec NOELSE {}
    | IF expression LBRACE statement_list RBRACE ELSE LBRACE statement_list RBRACE {}
    | FOR VAR ID ASSIGN LITERAL COLON LITERAL {}
    | WHILE expression LBRACE statement_list RBRACE {}

statement_list :
    /* nothing */ {}
    | statement_list statement {}

expression:
    LITERAL          {}
    | ID               {}
    | expression PLUS   expression {}
    | expression MINUS  expression {}
    | expression TIMES  expression {}
    | expression DIVIDE expression {}
    | expression EQ     expression {}
    | expression NEQ    expression {}
    | expression LT     expression {}
    | expression LEQ    expression {}
    | expression GT     expression {}
    | expression GEQ    expression {}
    | ID ASSIGN expression   {}
    | ID LPAREN arguments_opt RPAREN {} 
    | LPAREN expression RPAREN {}

arguments_opt:
    /* nothing */ {}
  | arguments_list  {}

arguments_list:
    expression        {}       
  | arguments_list COMMA expression {} 

primitive_type:
    VAR {}
    | VECTOR {}
    | VECSPACE {}
    | MATRIX {}
    | INSPACE {}
    | AFFSPACE {}


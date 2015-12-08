open Ast
open Past
open Parser

module StringMap = Map.Make(String)


let translate_op = function
    Add -> Padd
    | Sub -> Psub
    | Mult -> Pmult
    | Div -> Pdiv
    | Add_Dot -> Padd_Dot
    | Sub_Dot -> Psub_Dot
    | Mult_Dot -> Pmult_Dot
    | Div_Dot -> Pdiv_Dot
    | Equal -> Pequal
    | Neq -> Pneq
    | Less -> Pless
    | Leq -> Pleq
    | Greater -> Pgreater
    | Geq -> Pgeq
    | And -> Pand
    | Or -> Por

let translate_elem = function
    | Nid(s) -> P_nid(s)
    | Arrayid(s1, s2) -> P_arrayid(s1, s2)

let translate_builtin = function
    Dim -> P_dim
    | Size -> P_size
    | Vsconst -> P_vsconst

let rec translate_expr = function (*TODO: Add symbol table as argument*)
    Literal(l) -> P_literal(l)
    | Id(el) -> P_id(translate_elem el)
    | Transpose(e) -> P_transpose(translate_expr e)
    | Binop(e1, o, e2) -> P_binop(translate_expr e1, translate_op o, translate_expr e2)
    | Belongs(e1, e2) -> P_belongs(translate_expr e1, translate_expr e2)
    | LieBracket(e1, e2) -> P_lieBracket(translate_expr e1, translate_expr e2)
    | Inpro(id, e1, e2) -> P_inpro(id, translate_expr e1, translate_expr e2) 
    | Assign(v, e) -> P_assign(v, translate_expr e)
    | AssignArr(v, e) -> P_assignArr(v, List.map translate_expr e)
    | Call(f, el) -> P_call(f, List.map translate_expr el)
    | Builtin(el, s) -> P_builtin(translate_elem el, translate_builtin s)
    | Print(e) -> P_print(translate_expr e)
    | Noexpr -> P_noexpr
    
(* num_ident indicates the number of tabs at the begin of the statement, each tab is three spaces *)
let rec translate_stmt = function
    Block(stmts) -> P_block(List.map translate_stmt stmts)
    | Expr(expr) -> P_expr(translate_expr expr)
    | Return(expr) -> P_return(translate_expr expr) 
    | If(e, s1, s2) -> P_if(translate_expr e, List.map translate_stmt s1, List.map translate_stmt s2) 
    | For(l, a1, a2, s) -> P_for(l, translate_expr a1, translate_expr a2, List.map translate_stmt s) 
    | While(e, s) -> P_while(translate_expr e, List.map translate_stmt s)
    | Continue -> P_continue
    | Break -> P_break

let translate_prim_value = function
    VValue(s) -> P_Value(s)
    | VecValue(s) -> P_VecValue(s) 
    | MatValue(s) -> P_MatValue(s)
    | VecSpValue(s) -> P_VecSpValue(s)   
    | InSpValue(s1, s2) -> P_InSpValue(s1, s2)            
    | AffSpValue(s1, s2) -> P_AffSpValue(s1, s2) 
    | Expression(e) -> P_Expression(translate_expr e)
    | Notknown -> P_Notknown


let translate_prim_type = function
    Var -> P_var
    | Vector -> P_vector
    | Matrix -> P_matrix
    | VecSpace -> P_vecSpace
    | InSpace -> P_inSpace
    | AffSpace -> P_affSpace

let translate_normal_decl = function
    Vardecl(v) -> 
        P_Vardecl( 
        { p_vname = v.vname; 
          p_value = translate_prim_value v.value; 
          p_data_type = translate_prim_type v.data_type; 
          p_pos = v.pos})   
    | Arraydecl(a) -> 
        P_Arraydecl(
        { p_aname = a.aname; 
          p_elements = List.map translate_expr a.elements; 
          p_data_type = translate_prim_type a.data_type; 
          p_length = a.length; 
          p_pos = a.pos})

let translate_func_stmt = function
   Local(s) -> P_Local(translate_normal_decl s)
   | Body(s) -> P_Body(translate_stmt s)  

let translate_func_decl fdecl =
    { p_fname = fdecl.fname;
      p_params = List.map translate_normal_decl fdecl.params;
      p_body = List.map translate_func_stmt fdecl.body
    }

let translate_program_stmt = function
   Variable(v) -> P_Variable(translate_normal_decl v)
   | Function(f) -> P_Function(translate_func_decl f)

let translate program = 
    List.map translate_program_stmt program
 

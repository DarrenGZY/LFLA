(* operators *)
type pOp = Padd | Psub | Pmult | Pdiv | Padd_Dot | Psub_Dot | Pmult_Dot 
        | Pdiv_Dot | Pequal | Pneq | Pless | Pleq | Pgreater | Pgeq
        | Pand | Por  
(* 
 * element, normal id or 
 * array id with index 
 * *)
type pElem = 
  | P_nid of string  (* normal identifier *)
  | P_arrayid of string * string (* array identifier *)

(* primitive types, seperate 
 * noraml types and array types 
 * *)
type pPrim_type = 
    P_var
  | P_vector
  | P_matrix
  | P_vecSpace
  | P_inSpace
  | P_affSpace
  | P_varArr
  | P_vectorArr
  | P_matrixArr
  | P_vecSpaceArr
  | P_inSpaceArr
  | P_affSpaceArr
  | P_unit

(* expression *)
type pExpr =
    P_literal of string
  | P_id of pElem
  | P_binop of pExpr * pOp * pExpr
  | P_belongs of pExpr * pExpr
  | P_lieBracket of pExpr * pExpr
  | P_inpro of pExpr * pExpr * pExpr
  | P_transpose of pExpr
  | P_assign of pElem * pExpr
  | P_assignArr of pElem * pExpr list
  | P_call of string * pExpr list
  | P_print of pExpr
  | P_exprValue of pPrim_value
  | P_matrixMul of pExpr * pExpr
  | P_dim of pPrim_type * pExpr
  | P_size of pExpr
  | P_basis of pExpr
  | P_trace of pExpr
  | P_image of pExpr
  | P_rank of pExpr
  | P_evalue of pExpr
  | P_ceil of pExpr
  | P_floor of pExpr
  | P_sqrt of pExpr
  | P_solve of pExpr * pExpr
  | P_action of pExpr * pExpr
  | P_noexpr

(* value of primitive type *)
and pPrim_value = 
    P_Value of string
  | P_VecValue of string list
  | P_MatValue of string list list
  | P_VecSpValue of pExpr list
  | P_VecSpValueArr of pExpr list
  | P_InSpValue of pExpr * pExpr
  | P_AffSpValue of pExpr * pExpr
  | P_Expression of pExpr
  | P_Notknown

(* variable declaration
 * p_vname : name of variable
 * p_value : value of variable
 * p_data_type : type of variable
 * p_pos : position in original code(not used)
 * *)
type pVar_decl = {
    p_vname : string;
    p_value : pPrim_value;
    p_data_type : pPrim_type;
    p_pos : int;
}

(* array declaration
 * p_aname : name of array identifier
 * p_elements : expression list represents the elements of array
 * p_length : length of the array
 * p_data_type : type of variable
 * p_pos : position in original code(not used)
 * *)
type pArray_decl = {
    p_aname : string;
    p_elements : pExpr list;
    p_data_type : pPrim_type;
    p_length : int;
    p_pos : int;
}

(* combine variable declarations and array declarations *)
type pNormal_decl = 
    P_Vardecl of pVar_decl
  | P_Arraydecl of pArray_decl

(* statement *)
type pStmt =
    P_block of pStmt list
  | P_expr of pExpr
  | P_return of pExpr
  | P_if of pExpr * pStmt list * pStmt list
  | P_for of string * pExpr * pExpr * pStmt list
  | P_while of pExpr * pStmt list
  | P_continue
  | P_break
  | P_decl of pNormal_decl

(* function declaration 
 * p_fname : name of function
 * p_params : list of local normal declarations
 * p_body : main part of function, a list of statments
 * p_ret_type : return type of function
 *)
type pFunc_decl = {
    p_fname : string;
    p_params : pNormal_decl list;
    p_body : pStmt list;
}

(* combine gloval variable declaration and funciton declaration *)
type pProgram_stmt =
    P_Variable of pNormal_decl
  | P_Function of pFunc_decl

(* root of past *)
type pProgram = pProgram_stmt list

type pOp = Padd | Psub | Pmult | Pdiv | Padd_Dot | Psub_Dot | Pmult_Dot 
        | Pdiv_Dot | Pequal | Pneq | Pless | Pleq | Pgreater | Pgeq
        | Pand | Por  

(* store var type value *)
type pVar_value = 
    P_int of int
  | P_float of float 

(* store vector type value *)
type pVec_value = 
    pVar_value list

(* store matrix type value *)
type pMat_value = 
    pVar_value list list

(* store vector space type value *)
type pVecspace_value =
    pVec_value list

type pElem = 
  | P_nid of string  (* normal identifier *)
  | P_arrayid of string * string (* array identifier *)
(*
type pBuiltin_func = 
    P_dim
  | P_size
 (* | P_vsconst *)
  | P_basis
  | P_trace
  | P_image
  | P_rank
  | P_evalue
*)
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

type pExpr =
    P_literal of string
  | P_id of pElem
  | P_binop of pExpr * pOp * pExpr
  | P_belongs of pExpr * pExpr
  | P_lieBracket of pExpr * pExpr
  | P_inpro of pExpr * pExpr * pExpr
  | P_transpose of pExpr
  | P_assign of string * pExpr
  | P_assignArr of string * pExpr list
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

type pVar_decl = {
    p_vname : string;
    p_value : pPrim_value;
    p_data_type : pPrim_type;
    p_pos : int;
}

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

type pFunction_stmt =
    P_Local of pNormal_decl
  | P_Body of pStmt

type pFunc_decl = {
    p_fname : string;
    p_params : pNormal_decl list;
    p_body : pStmt list;
}

type pProgram_stmt =
    P_Variable of pNormal_decl
  | P_Function of pFunc_decl

type pProgram = pProgram_stmt list

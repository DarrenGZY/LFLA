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

let translate_elem env = function
    | Nid(s) -> 
        let global_vars, global_funcs, local_vars = env in
        if StringMap.mem s global_vars then
            P_nid(s)
        else if StringMap.mem s local_vars then
            P_nid(s)
        else raise(Failure ("undeclared identifier " ^ s))
    | Arrayid(s1, s2) -> 
        let global_vars, global_funcs, local_vars = env in 
        if StringMap.mem s1 global_vars then
            P_arrayid(s1, s2)
        else if StringMap.mem s1 local_vars then
            P_arrayid(s1, s2)
        else raise(Failure ("undeclared identifier " ^ s1))

let translate_builtin = function
    Dim -> P_dim
    | Size -> P_size
    | Vsconst -> P_vsconst

(* env = (global_var, global_funcs, local_vars) *)

let rec traverse_exprs env = function
    [] -> [], env
    | hd::tl -> 
        let pE, env = translate_expr env hd in
        let pTl, env = traverse_exprs env tl in
        pE::pTl, env
and translate_expr env = function (*TODO: Add symbol table as argument*)
    Literal(l) -> (P_literal(l), env)
    | Id(el) -> 
        (P_id(translate_elem env el), env)
    | Transpose(e) -> 
        let (pExpr, env) = translate_expr env e in
            (P_transpose(pExpr), env)
    | Binop(e1, o, e2) -> 
        let (pE1, env) = translate_expr env e1 in
        let pO = translate_op o in
        let (pE2, env) = translate_expr env e2 in
            (P_binop(pE1, pO, pE2), env)
    | Belongs(e1, e2) -> 
        let (pE1, env) = translate_expr env e1 in
        let (pE2, env) = translate_expr env e2 in
            (P_belongs(pE1, pE2), env)
    | LieBracket(e1, e2) -> 
        let (pE1, env) = translate_expr env e1 in
        let (pE2, env) = translate_expr env e2 in
            (P_lieBracket(pE1, pE2), env)
    | Inpro(id, e1, e2) -> 
        let global_vars, global_funcs, local_vars = env in
            if (not (StringMap.mem id global_vars)) && (not (StringMap.mem id local_vars)) then
               raise(Failure("undefined innner product identifier"))
            else
                let pE1, env = translate_expr env e1 in 
                let pE2, env = translate_expr env e2 in
                    P_inpro(id, pE1, pE2), env 
    | Assign(id, e) -> 
        let global_vars, global_funcs, local_vars = env in
            if (not (StringMap.mem id global_vars)) && (not (StringMap.mem id local_vars)) then
               raise(Failure("undefined identifier"))
            else
                let pE, env = translate_expr env e in
                    P_assign(id, pE), env
    | AssignArr(id, e) -> 
        let global_vars, global_funcs, local_vars = env in
            if (not (StringMap.mem id global_vars)) && (not (StringMap.mem id local_vars)) then
                raise(Failure("undefined identifier"))
            else
                let pE, env = traverse_exprs env e in
                    P_assignArr(id, pE), env
    | Call(f, el) -> 
        let global_vars, global_funcs, local_vars = env in
            if (not (StringMap.mem f global_funcs))  then
                raise(Failure("undefined funciton"))
            else
                let pE, env = traverse_exprs env el in
                    (P_call(f, pE), env)
    | Builtin(el, s) -> 
        (P_builtin(translate_elem env el, translate_builtin s), env)
    | Print(e) -> let pE, env = translate_expr env e in
                    P_print(pE), env
    | Noexpr -> P_noexpr, env
    
let rec traverse_stmts env = function
    [] -> [], env
    | hd::tl -> 
        let pStmt, env = translate_stmt env hd in
        let pTl, env = traverse_stmts env tl in
            pStmt::pTl, env
and translate_stmt env= function
    Block(stmts) -> 
        let pStmts, env = traverse_stmts env stmts in
        P_block(pStmts), env
    | Expr(expr) -> 
        let pExpr, env = translate_expr env expr in
            P_expr(pExpr), env
    | Return(expr) -> 
        let pExpr, env = translate_expr env expr in
            P_return(pExpr), env 
    | If(e, s1, s2) -> 
        let pExpr, env = translate_expr env e in
        let pStmts1, env = traverse_stmts env s1 in
        let pStmts2, env = traverse_stmts env s2 in
            P_if(pExpr, pStmts1, pStmts2), env 
    | For(l, a1, a2, s) ->
        let pExpr1, env = translate_expr env a1 in
        let pExpr2, env = translate_expr env a2 in
        let pStmts, env = traverse_stmts env s in
            P_for(l, pExpr1, pExpr2, pStmts), env 
    | While(e, s) -> 
        let pExpr, env = translate_expr env e in
        let pStmts, env = traverse_stmts env s in
            P_while(pExpr, pStmts), env
    | Continue -> P_continue, env
    | Break -> P_break, env

(*TODO: if prim value is needed *)
let translate_prim_value env = function
    VValue(s) -> P_Value(s), env
    | VecValue(s) -> P_VecValue(s), env 
    | MatValue(s) -> P_MatValue(s), env
    | VecSpValue(s) -> P_VecSpValue(s), env   
    | InSpValue(s1, s2) -> P_InSpValue(s1, s2), env            
    | AffSpValue(s1, s2) -> P_AffSpValue(s1, s2), env 
    | Expression(e) -> 
        let pExpr, env = translate_expr env e in
            P_Expression(pExpr), env
    | Notknown -> P_Notknown, env


let translate_prim_type = function
    Var -> P_var
    | Vector -> P_vector
    | Matrix -> P_matrix
    | VecSpace -> P_vecSpace
    | InSpace -> P_inSpace
    | AffSpace -> P_affSpace


let translate_local_normal_decl env local_var = 
    match local_var with
    Lvardecl(v) ->
        let pValue, env = translate_prim_value env v.value in
        let p_var =  {  p_vname = v.vname; 
                        p_value = pValue; 
                        p_data_type = translate_prim_type v.data_type; 
                        p_pos = v.pos   } 
        in
        let global_vars, global_funcs, local_vars = env in
            let local_vars = 
                if (not (StringMap.mem v.vname local_vars)) then
                    StringMap.add v.vname local_var local_vars
                else
                    raise(Failure("Already defined variable" ^ v.vname))
            in
            P_Vardecl(p_var), (global_vars, global_funcs, local_vars)
           
    | Larraydecl(a) -> 
        let pExprs, env = traverse_exprs env a.elements in
        let p_array = { p_aname = a.aname; 
                        p_elements = pExprs;
                        p_data_type = translate_prim_type a.data_type; 
                        p_length = a.length;
                        p_pos = a.pos   } 
        in
        let global_vars, global_funcs, local_vars = env in
            let local_vars = 
                if (not (StringMap.mem a.aname local_vars)) then
                    StringMap.add a.aname local_var local_vars
                else
                    raise(Failure("Already defined variable" ^ a.aname))
            in
            P_Arraydecl(p_array), (global_vars, global_funcs, local_vars)

let translate_global_normal_decl env global_var = 
    match global_var with
    Gvardecl(v) ->
        let pValue, env = translate_prim_value env v.value in
        let p_var =  {  p_vname = v.vname; 
                        p_value = pValue; 
                        p_data_type = translate_prim_type v.data_type; 
                        p_pos = v.pos   } 
        in
        let global_vars, global_funcs, local_vars = env in
            let global_vars = 
                if (not (StringMap.mem v.vname global_vars)) then
                    StringMap.add v.vname global_var global_vars
                else
                    raise(Failure("Already defined variable" ^ v.vname))
            in
            P_Vardecl(p_var), (global_vars, global_funcs, local_vars)
           
    | Garraydecl(a) -> 
        let pExprs, env = traverse_exprs env a.elements in
        let p_array = { p_aname = a.aname; 
                        p_elements = pExprs;
                        p_data_type = translate_prim_type a.data_type; 
                        p_length = a.length;
                        p_pos = a.pos   } 
        in
        let global_vars, global_funcs, local_vars = env in
            let global_vars = 
                if (not (StringMap.mem a.aname global_vars)) then
                    StringMap.add a.aname global_var global_vars
                else
                    raise(Failure("Already defined variable" ^ a.aname))
            in
            P_Arraydecl(p_array), (global_vars, global_funcs, local_vars)


let rec traverse_local_vars env = function
    [] -> [], env
    | hd::tl ->
        let p_local, env = translate_local_normal_decl env hd in
        let p_tl, env = traverse_local_vars env tl in
        p_local::p_tl, env

let translate_func_stmt env = function
   Local(s) -> 
        let pVar, env = translate_local_normal_decl env s in
            P_Local(pVar), env 
   | Body(s) -> 
        let pStmt, env = translate_stmt env s in
            P_Body(pStmt), env  

let rec traverse_func_stmts env = function
    [] -> [], env
   | hd::tl ->
        let p_func_stmt, env = translate_func_stmt env hd in
        let p_tl, env = traverse_func_stmts env tl in
        (p_func_stmt::p_tl), env

let translate_func_decl env fdecl =
    let global_vars, global_funcs, local_vars = env in
        if (not (StringMap.mem fdecl.fname global_funcs)) then
            let pParams, env = traverse_local_vars env fdecl.params in
            let pStmts, env = traverse_func_stmts env fdecl.body in
            let global_vars, global_funcs, local_vars = env in
                let global_funcs = StringMap.add fdecl.fname fdecl global_funcs
                in
                {
                    p_fname = fdecl.fname;
                    p_params = pParams; 
                    p_body = pStmts 
                }, (global_vars, global_funcs, local_vars)
        else
            raise(Failure("Already defined function " ^ fdecl.fname))

let translate_program_stmt env = function
   Variable(v) -> let variable, env = translate_global_normal_decl env v
                    in P_Variable(variable), env
   | Function(f) -> let func, env = translate_func_decl env f
                    in P_Function(func), env

let translate program =
    (*let getName = function
        Variable(v) -> 
            (match v with
                Vardecl(var) -> var.vname
                | Arraydecl(arr) -> arr.aname
            )
        | Function(f) -> f.fname
    in
    let whole_table = 
        List.fold_left (fun m decl -> StringMap.add (getName decl) 0 m) StringMap.empty program
    in
    if StringMap.mem "main" whole_table then*)
        let rec traverse_program env = function
            [] -> [], env
            | hd::tl -> 
                    let p_program, env = translate_program_stmt env hd in
                    let p_tl, env = traverse_program env tl in
                        p_program::p_tl, env
        in
        traverse_program (StringMap.empty, StringMap.empty, StringMap.empty) program
    (*else
        raise (Failure("no main function")) *)
 

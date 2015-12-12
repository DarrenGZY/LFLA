open Ast
open Past
open Check

module StringMap = Map.Make(String)

(* translate ast operator to python ast operator *)
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

(* translate ast element to python ast element, and need to check symbol tables *)
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

(* translate ast builtin functions to python ast builtin functions *)
let translate_builtin = function
    Dim -> P_dim
    | Size -> P_size
    | Basis -> P_basis
(* env = (global_var, global_funcs, local_vars) *)
(* traverse_exprs works to translate a list of expression *)
let rec traverse_exprs env = function
    [] -> [], env
    | hd::tl -> 
        let pE, env = translate_expr env hd in
        let pTl, env = traverse_exprs env tl in
        pE::pTl, env
(* translate ast expr to python ast expr and update symbol tables *)
and translate_expr env = function 
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
    | Assign(id, e) -> (* TODO: update the id in symbol table *) 
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
   (* | Vsconst(e) -> let pE, env = traverse_exprs env e in 
        P_vsconst(pE), env *)
    | ExprValue(v) -> 
        let pV, env = translate_prim_value env v in
            P_exprValue(pV), env
    | Noexpr -> P_noexpr, env

(*TODO: if prim value is needed *)
and translate_prim_value env = function
    VValue(s) -> P_Value(s), env
    | VecValue(s) -> P_VecValue(s), env 
    | MatValue(s) -> P_MatValue(s), env
    | VecSpValue(eList) -> 
            let pEList,env = traverse_exprs env eList in
                if check_list env Vector eList then
                        P_VecSpValue(pEList), env
                else if (List.length eList == 1) then
                    if check_list env VectorArr eList then
                        P_VecSpValueArr(pEList), env
                    else
                        raise(Failure("in vsconst fail in type checking"))
                else
                    raise(Failure("in vsconst fail in type checking"))
    | InSpValue(e1, e2) -> 
            let pE1, env = translate_expr env e1 in
            let pE2, env = translate_expr env e2 in
                let typ1 = type_of env e1 in
                let typ2 = type_of env e2 in
                    if typ1 <> VectorArr || typ2 <> Matrix then
                        raise(Failure("in InSpace construct fail in type checking"))
                    else    
                        P_InSpValue(pE1, pE2), env            
    | AffSpValue(e1, e2) -> 
            let pE1, env = translate_expr env e1 in
            let pE2, env = translate_expr env e2 in
                let typ1 = type_of env e1 in
                let typ2 = type_of env e2 in
                    if typ1 <> Vector || typ2 <> VecSpace then
                        raise(Failure("in AffSpace construct fail in type checking"))
                    else
                        P_AffSpValue(pE1, pE2), env 
    | Expression(typ, e) -> 
        let pExpr, env = translate_expr env e in
            let typ' = type_of env e in
                if typ' <> typ then
                    raise(Failure("in construct fail in type checking"))
                else
                    P_Expression(pExpr), env
    | Notknown -> P_Notknown, env
(* traverse_stmts works to translate a list of statements *)
let rec traverse_stmts env = function
    [] -> [], env
    | hd::tl -> 
        let pStmt, env = translate_stmt env hd in
        let pTl, env = traverse_stmts env tl in
            pStmt::pTl, env
(* translate ast stmt to python ast statement *)
and translate_stmt env= function
    Block(stmts) -> 
        let pStmts, env = traverse_stmts env stmts in
        P_block(pStmts), env
    | Expr(expr) -> 
        let pExpr, env = translate_expr env expr in
        let _ = type_of env expr in
            P_expr(pExpr), env
    | Return(expr) -> 
        let pExpr, env = translate_expr env expr in
        let _ = type_of env expr in
            P_return(pExpr), env 
    | If(e, s1, s2) -> 
        let pExpr, env = translate_expr env e in
            let typ = type_of env e in
                if typ <> Var then
                    raise(Failure(" condition in if should be var type"))
                else

                    let pStmts1, env = traverse_stmts env s1 in
                    let pStmts2, env = traverse_stmts env s2 in
                        P_if(pExpr, pStmts1, pStmts2), env 
    | For(l, a1, a2, s) ->
        let pExpr1, env = translate_expr env a1 in
        let pExpr2, env = translate_expr env a2 in
            let typ1 = type_of env a1 in
            let typ2 = type_of env a2 in
                if typ1 <> Var || typ2 <> Var then
                    raise(Failure(" condition in for should be var type"))
                else
                    let pStmts, env = traverse_stmts env s in
                        P_for(l, pExpr1, pExpr2, pStmts), env 
    | While(e, s) -> 
        let pExpr, env = translate_expr env e in
            let typ = type_of env e in
                if typ <> Var then
                    raise(Failure(" condition in while should be var type"))
                else
                    let pStmts, env = traverse_stmts env s in
                        P_while(pExpr, pStmts), env
    | Continue -> P_continue, env
    | Break -> P_break, env


(* translate ast prim type to python ast prim type *)
let translate_prim_type = function
    Var -> P_var
    | Vector -> P_vector
    | Matrix -> P_matrix
    | VecSpace -> P_vecSpace
    | InSpace -> P_inSpace
    | AffSpace -> P_affSpace
    | VarArr -> P_varArr
    | VectorArr -> P_vectorArr
    | MatrixArr -> P_matrixArr
    | VecSpaceArr -> P_vecSpaceArr
    | InSpaceArr -> P_inSpaceArr
    | AffSpaceArr -> P_affSpaceArr
    | Unit -> P_unit
(* translate local variables to python ast variables *)
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
                    raise(Failure("Already defined variable  " ^ v.vname))
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
                    raise(Failure("Already defined variable " ^ a.aname))
            in
            P_Arraydecl(p_array), (global_vars, global_funcs, local_vars)

(* translate global variables to python ast variables *)
let translate_global_normal_decl env global_var = 
    let global_vars, global_funcs = env in
    match global_var with
    Gvardecl(v) ->
        let pValue, env = translate_prim_value (global_vars, global_funcs, StringMap.empty) v.value 
        in                                              (* for global variable, local_vars table is empty *)
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
                    raise(Failure("Already defined variable " ^ v.vname))
            in
            P_Vardecl(p_var), (global_vars, global_funcs)
           
    | Garraydecl(a) -> 
        let pExprs, env = traverse_exprs (global_vars, global_funcs, StringMap.empty) a.elements 
        in                                              (* for global array, local_vars table is empty*)
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
                    raise(Failure("Already defined variable " ^ a.aname))
            in
            P_Arraydecl(p_array), (global_vars, global_funcs)

(* translate a list of local variables *)
let rec traverse_local_vars env = function
    [] -> [], env
    | hd::tl ->
        let p_local, env = translate_local_normal_decl env hd in
        let p_tl, env = traverse_local_vars env tl in
        p_local::p_tl, env

(* translate function statements *)
let translate_func_stmt env = function
   Local(s) -> 
        let pVar, env = translate_local_normal_decl env s in
            P_Local(pVar), env 
   | Body(s) -> 
        let pStmt, env = translate_stmt env s in
            P_Body(pStmt), env  

(* translate a list of funciton statements *)
let rec traverse_func_stmts env = function
    [] -> [], env
   | hd::tl ->
        let p_func_stmt, env = translate_func_stmt env hd in
        let p_tl, env = traverse_func_stmts env tl in
        (p_func_stmt::p_tl), env

(* translate function declaration and update symbol tables *)
let translate_func_decl env fdecl =
    let global_vars, global_funcs = env in
        if (not (StringMap.mem fdecl.fname global_funcs)) then
            let pParams, env = traverse_local_vars (global_vars, global_funcs, StringMap.empty) fdecl.params in (* give empty local_vars table *)
            let pStmts, env = traverse_func_stmts env fdecl.body in
            let global_vars, global_funcs, local_vars = env in
                let global_funcs = StringMap.add fdecl.fname fdecl global_funcs
                in
                {
                    p_fname = fdecl.fname;
                    p_params = pParams; 
                    p_body = pStmts 
                }, (global_vars, global_funcs)
        else
            raise(Failure("Already defined function " ^ fdecl.fname))

(* translate program statements *)
let translate_program_stmt env = function
   Variable(v) -> let variable, env = translate_global_normal_decl env v
                    in P_Variable(variable), env
   | Function(f) -> let func, env = translate_func_decl env f
                    in P_Function(func), env

let translate program =
    let rec traverse_program env = function
        [] -> [], env
        | hd::tl -> 
            let p_program, env = translate_program_stmt env hd in
            let p_tl, env = traverse_program env tl in
                p_program::p_tl, env
    in
    let p_program, (global_vars, global_funcs) 
        = traverse_program (StringMap.empty, StringMap.empty) program  (* give empty global_vars and global_funcs symbol table *)
    in 
        if (not (StringMap.mem "main" global_funcs)) then 
            raise (Failure("no main function")) 
        else
            p_program, (global_vars, global_funcs)

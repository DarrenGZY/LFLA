open Ast
open Past
open Check
open Translate_env

(* module StringMap = Map.Make(String) *)

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

(* translate ast element to python ast element, and need to check symbol tables *)
let translate_elem env = function
    | Nid(s) -> 
        if is_defined_var s env then
            P_nid(s)
        else raise(Failure ("undeclared identifier " ^ s))
    | Arrayid(s1, s2) -> 
        if is_defined_var s1 env then
            P_arrayid(s1, s2)
        else raise(Failure ("undeclared identifier " ^ s1))

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
    | Binop(e1, o, e2) -> 
        let (pE1, env) = translate_expr env e1 in
        let pO = translate_op o in
        let (pE2, env) = translate_expr env e2 in
        (match (type_of env e1, o, type_of env e2) with
            Matrix, Mult_Dot, Matrix -> (P_matrixMul(pE1, pE2), env)
            |_,_,_ -> (P_binop(pE1, pO, pE2), env))
    | Assign(id, e) -> (* TODO: update the id in symbol table *) 
            if not (is_defined_var id env) then
               raise(Failure("undefined identifier"))
            else
                let pE, env = translate_expr env e in
                    P_assign(id, pE), env
    | AssignArr(id, e) -> 
            if not (is_defined_var id env) then
                raise(Failure("undefined identifier"))
            else
                let pE, env = traverse_exprs env e in
                    P_assignArr(id, pE), env
    | Call(f, el) -> 
            if not (is_func f env) then
                raise(Failure("undefined funciton"))
            else
                let pE, env = traverse_exprs env el in
                    (P_call(f, pE), env)
    | Callbuiltin(f, el) -> (*TODO: check the builtin function types *) 
            if (List.length el) == 0 then
                raise(Failure("wrong arguments in builtin funciton"))
            else
            let pElist, env = traverse_exprs env el in
            (match f with
            | Sqrt -> P_sqrt(List.hd pElist), env
            | Ceil -> P_ceil(List.hd pElist), env
            | Floor -> P_floor(List.hd pElist), env 
            | Dim -> 
                    let pE = List.hd pElist in 
                    let typ = type_of env (List.hd el)in
                    P_dim( translate_prim_type typ, pE), env
            | Size -> P_size(List.hd pElist), env
            | Basis -> P_basis(List.hd pElist), env
            | Image -> P_image(List.hd pElist), env
            | Rank -> P_rank(List.hd pElist), env
            | Trace -> P_trace(List.hd pElist), env
            | Evalue -> P_evalue(List.hd pElist), env
            | Solve -> 
                    if (List.length pElist) <> 2 then
                        raise(Failure("wrong arguments in builtin function"))
                    else P_solve(List.hd pElist, List.nth pElist 1), env
            | Belongs -> 
                    if (List.length pElist) <> 2 then
                        raise(Failure("wrong arguments in builtin funciton"))
                    else P_belongs(List.hd pElist, List.nth pElist 1), env
            | LieBracket ->
                    if (List.length pElist) <> 2 then
                        raise(Failure("wrong arguments in builtin function"))
                    else P_lieBracket(List.hd pElist, List.nth pElist 1), env
            | Inpro ->
                    if (List.length pElist) <> 3 then
                        raise(Failure("wrong arguments in builtin function"))
                    else P_inpro(List.hd pElist, List.nth pElist 1, List.nth pElist 2), env
            | Transpose -> P_transpose(List.hd pElist), env
            | Action -> 
                    if (List.length pElist) <> 2 then
                        raise(Failure("wrong arguments in builtin function"))
                    else P_action(List.hd pElist, List.nth pElist 1), env
            | Print -> P_print(List.hd pElist), env
            )  
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
        let vars' = 
            if (not (is_defined_var v.vname env)) then
                StringMap.add v.vname local_var env.scope.vars
            else
                raise(Failure("Already defined variable  " ^ v.vname))
        in
        let scope' = { env.scope with vars = vars' } in
        let env' = { env with scope = scope' } in
        P_Vardecl(p_var), env'
           
    | Larraydecl(a) -> 
        let length = List.length a.elements in
        if length <> a.length then
            raise(Failure("array length not match"))
        else
        let pExprs, env = traverse_exprs env a.elements in
        if not (check_list env (real_type a.data_type) a.elements) then (* check if array elements have right type *)
            raise(Failure("array elements have wrong type"))
        else
        let p_array = { p_aname = a.aname; 
                        p_elements = pExprs;
                        p_data_type = translate_prim_type a.data_type; 
                        p_length = a.length;
                        p_pos = a.pos   } 
        in
        let vars' = 
            if (not (is_defined_var a.aname env)) then
                StringMap.add a.aname local_var env.scope.vars
            else
                raise(Failure("Already defined variable  " ^ a.aname))
        in
        let scope' = { env.scope with vars = vars' } in
        let env' = { env with scope = scope' } in
        P_Arraydecl(p_array), env'


(* translate global variables to python ast variables *)
let translate_global_normal_decl env global_var = 
    match global_var with
    Gvardecl(v) ->
        let pValue, env = translate_prim_value env v.value 
        in                                              (* for global variable, local_vars table is empty *)
        let p_var =  {  p_vname = v.vname; 
                        p_value = pValue; 
                        p_data_type = translate_prim_type v.data_type; 
                        p_pos = v.pos   } 
        in
        let global_vars' = 
            if (not (is_global_var v.vname env)) then
                StringMap.add v.vname global_var env.global_vars
            else
                raise(Failure("Already defined variable " ^ v.vname))
        in
        let env' = { env with global_vars = global_vars' } in 
        P_Vardecl(p_var), env'
           
    | Garraydecl(a) -> 
        let length = List.length a.elements in
        if length <> a.length then
            raise(Failure("array length not match"))
        else
        let pExprs, env = traverse_exprs env a.elements 
        in
        if not (check_list env (real_type a.data_type) a.elements) then  (* check for each element if it has right type *)
           raise(Failure("array elements have wrong type"))
        else                                                    (* for global array, local_vars table is empty*)
        let p_array = { p_aname = a.aname; 
                        p_elements = pExprs;
                        p_data_type = translate_prim_type a.data_type; 
                        p_length = a.length;
                        p_pos = a.pos} 
        in
            let global_vars' = 
                if (not (is_global_var a.aname env)) then
                    StringMap.add a.aname global_var env.global_vars
                else
                    raise(Failure("Already defined variable " ^ a.aname))
            in
            let env' = { env with global_vars = global_vars' } in
            P_Arraydecl(p_array), env'

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
        let scope' = { parent = Some(env.scope); vars = StringMap.empty } in
        let env' = { env with scope = scope' } in
        let pStmts, env' = traverse_stmts env' stmts in
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
                    let scope' = { parent = Some(env.scope); vars = StringMap.empty } in
                    let env' = { env with scope = scope' } in
                    let pStmts1, _ = traverse_stmts env' s1 in
                    let pStmts2, _ = traverse_stmts env' s2 in
                        P_if(pExpr, pStmts1, pStmts2), env 
    | For(l, a1, a2, s) ->
        let typ = type_of_id env l in
        if typ <> Var then
            raise(Failure("variable in for should be var type"))
        else
        let pExpr1, env = translate_expr env a1 in
        let pExpr2, env = translate_expr env a2 in
            let typ1 = type_of env a1 in
            let typ2 = type_of env a2 in
                if typ1 <> Var || typ2 <> Var then
                    raise(Failure("condition in for should be var type"))
                else
                    let scope' = { parent = Some(env.scope); vars = StringMap.empty } in
                    let env' = { env with scope = scope'; in_for = true } in
                    let pStmts, _  = traverse_stmts env' s in
                        P_for(l, pExpr1, pExpr2, pStmts), env 
    | While(e, s) -> 
        let pExpr, env = translate_expr env e in
            let typ = type_of env e in
                if typ <> Var then
                    raise(Failure(" condition in while should be var type"))
                else
                    let scope' = { parent = Some(env.scope); vars = StringMap.empty } in
                    let env' = { env with scope = scope'; in_while = true } in
                    let pStmts, _ = traverse_stmts env' s in
                        P_while(pExpr, pStmts), env
    | Continue -> P_continue, env
    | Break -> P_break, env
    | Decl(d) -> 
            let pD, env = translate_local_normal_decl env d in 
            P_decl(pD), env

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
    if (not (is_func fdecl.fname env)) then
        let pParams, env = traverse_local_vars env fdecl.params in (* give empty local_vars table *)
        let pStmts, env = traverse_stmts env fdecl.body in
        let global_funcs' = StringMap.add fdecl.fname fdecl env.global_funcs in
        let env' = { env with global_funcs = global_funcs' }    
        in
        {
            p_fname = fdecl.fname;
            p_params = pParams; 
            p_body = pStmts 
        }, env'
    else
        raise(Failure("Already defined function " ^ fdecl.fname))

(* translate program statements *)
let translate_program_stmt env = function
   Variable(v) -> let variable, env = translate_global_normal_decl env v
                    in P_Variable(variable), env
   | Function(f) -> 
           let scope' = { parent = Some({ parent=None; vars=StringMap.empty}); vars = StringMap.empty } in
           let env' = { env with scope = scope' } in
           let func, env' = translate_func_decl env' f
                    in P_Function(func), env'

let translate program =
    let rec traverse_program env = function
        [] -> [], env
        | hd::tl -> 
            let p_program, env = translate_program_stmt env hd in
            let p_tl, env = traverse_program env tl in
                p_program::p_tl, env
    in
    let scope' = { parent = None; vars = StringMap.empty } in
    let env = { 
                scope = scope'; 
                global_vars = StringMap.empty; 
                global_funcs = StringMap.empty;
                return_type = Unit;
                in_while = false;
                in_for = false;
             }
    in 
    let p_program, env' 
        = traverse_program env program  (* give empty global_vars and global_funcs symbol table *)
    in 
        if (not (is_func "main" env')) then 
            raise (Failure("no main function")) 
        else
            p_program 

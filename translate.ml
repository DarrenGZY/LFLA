open Ast
open Past
open Check
open Translate_env

(* module StringMap = Map.Make(String) *)

(* input : ast operator
 * output : past operator
 * translate ast operator to python ast operator 
 * *)
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

(* input : ast operator
 * output : past operator
 * translate ast prim type to python ast prim type *)
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

(* input: ast element
 * output: past element
 * translate ast element to python ast element, and
 * check symbol tables, throw exception if they are not defined
 * *)
let translate_elem env = function
    | Nid(s) -> 
        if is_defined_var s env then
            P_nid(s)
        else raise(Failure ("undeclared identifier " ^ s))
    | Arrayid(s1, s2) -> 
        if is_defined_var s1 env then
            P_arrayid(s1, s2)
        else raise(Failure ("undeclared identifier " ^ s1))

(* input: env->translate_env and ast expression list
 * output: past expression list and updated env
 * traverse_exprs works to translate a list of expression 
 * *)
let rec traverse_exprs env = function
    [] -> [], env
    | hd::tl -> 
        let pE, env = translate_expr env hd in
        let pTl, env = traverse_exprs env tl in
        pE::pTl, env
(* input: ast expression and translate environment
 * output: past expression and updated environment
 * translate ast expr to python ast expr and do some basic checks
 * *)
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
    | Assign(el, e) -> (* TODO: update the id in symbol table *) 
            if not (is_defined_element el env) then
               raise(Failure("undefined identifier"))
            else
                let pE, env = translate_expr env e in
                let pEl = translate_elem env el in    
                    P_assign(pEl, pE), env
    | AssignArr(el, e) -> 
            if not (is_defined_element el env) then
                raise(Failure("undefined identifier"))
            else
                let pE, env = traverse_exprs env e in
                let pEl = translate_elem env el in    
                    P_assignArr(pEl, pE), env
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

(* input: ast prim_value and translate environment
 * output: past prim_value and updated environment
 * translate ast prim_value to past prim_value
 * do type checking during translation
 * *)
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
   
(* input: ast local declaration and translate environment
 * output: past local declaration and updated environment
 * translate local variables to python ast variables *)
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
        if length <> 0 && length <> a.length then  (* length = 0 occurs only when a is function param *)
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

(* input: ast global declaration and translate environment
 * output: past global declaration and updated environment
 * translate global variables to python ast variables *)
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
        if length <> 0 && length <> a.length then  (* length = 0 occurs only when a is a function param *)
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

(* input: return expression list, 
 *        function body statements,
 *        translate environment
 * output: return expression list
 * collect all return expressions in function
 * *)
let rec find_return_exprs env ret_exprs body= 
    match body with
        [] -> ret_exprs
        | hd::tl ->
                (match hd with 
                    Return(e) -> 
                        let ret_exprs' = e::ret_exprs in
                            find_return_exprs env ret_exprs' tl
                    | Block(stmts) -> 
                            let ret_exprs1 = find_return_exprs env ret_exprs stmts in
                            find_return_exprs env ret_exprs1 tl
                    | Expr(e) -> 
                            find_return_exprs env ret_exprs tl
                    | If(e, s1, s2) ->
                            let ret_exprs1 = find_return_exprs env ret_exprs s1 in
                            let ret_exprs2 = find_return_exprs env ret_exprs1 s2 in
                                find_return_exprs env ret_exprs2 tl
                    | While(e, s) ->
                            let ret_exprs1 = find_return_exprs env ret_exprs s in
                                find_return_exprs env ret_exprs1 tl
                    | For(v, e1, e2, s) ->
                            let ret_exprs1 = find_return_exprs env ret_exprs s in
                                find_return_exprs env ret_exprs1 tl
                    | Continue -> 
                            find_return_exprs env ret_exprs tl
                    | Break ->
                            find_return_exprs env ret_exprs tl
                    | Decl(l) ->
                            find_return_exprs env ret_exprs tl  
                     
                )
(* input: return expression list,
 *        translate environment
 * output: prim_type 
 * find the return type and check the consistancy
 * *)
let find_return_type env ret_exprs =
    if (List.length ret_exprs) == 0 then
        Unit
    else
    let expr = List.hd ret_exprs in
    let ret_typ = type_of env expr in
        if (check_list env ret_typ ret_exprs) then 
            ret_typ
        else
            raise(Failure "function return type don't match")

(* input: ast statments list
 *        translate environment
 * output: past statements list
 * translate a list of statements 
 * *)
let rec traverse_stmts env = function
    [] -> [], env
    | hd::tl -> 
        let pStmt, env = translate_stmt env hd in
        let pTl, env = traverse_stmts env tl in
            pStmt::pTl, env
(* input: ast statement
 *        translate environment
 * output: past statement
 * translate ast stmt to python ast statement 
 * do type checking during translation
 * *)
and translate_stmt env= function
    Block(stmts) ->
        let scope' = { parent = Some(env.scope); vars = StringMap.empty } in
        let env' = { env with scope = scope' } in
        let pStmts, env' = traverse_stmts env' stmts in
            P_block(pStmts),  env 
    | Expr(expr) -> 
        let pExpr, env = translate_expr env expr in
        let _ = type_of env expr in
            P_expr(pExpr), env
    | Return(expr) -> 
        let pExpr, env = translate_expr env expr in
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
                        (*let env = { env with return_type = ret_typ1 } in *)
                            P_if(pExpr, pStmts1, pStmts2), env  
    | For(l, a1, a2, s) ->
        let typ,_ = type_of_id env l in
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
                    let pStmts, _  = traverse_stmts  env' s in
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
    | Continue -> 
            if (not env.in_while) && (not env.in_for) then
                raise(Failure(" continue doesn't appear in a for loop or while loop"))
            else
                P_continue, env
    | Break -> 
            if (not env.in_while) && (not env.in_for) then
                raise(Failure(" continue doesn't appear in a for loop or while loop"))
            else
                P_break, env
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

(* input: ast function declaration 
 *        translate environment
 * output: past function declaration
 * translate function declaration 
 * and update symbol tables *)
let translate_func_decl env fdecl =
    if (not (is_func fdecl.fname env)) then
        let pParams, env = traverse_local_vars env fdecl.params in (* give empty local_vars table *)
        let pStmts, env = traverse_stmts env fdecl.body in
        let ret_exprs = find_return_exprs env [] fdecl.body in
        let ret_typ = find_return_type env ret_exprs in
        let fdecl' = { fdecl with ret_type = ret_typ } in
        let global_funcs' = StringMap.add fdecl'.fname fdecl' env.global_funcs in
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
(* input: whole ast
 * output: whole past
 * translate the whole ast to past
 * *)
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

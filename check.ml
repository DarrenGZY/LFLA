open Ast
open Translate_env

(* input: string of identifier
 *        trsnalte environment
 * output: (type, length)
 * if it is an array identifier, return length of array,
 * otherwise return 0.
 * *) 
let type_of_id env s = 
    if is_local_var s env then
        let decl = find_local_var s env in
            match decl with
                Lvardecl(var) -> (var.data_type, 0)
                | Larraydecl(arr) -> (arr.data_type, arr.length)
    else if is_global_var s env then
        let decl = find_global_var s env in
            match decl with
                Gvardecl(var) -> (var.data_type, 0)
                | Garraydecl(arr) -> (arr.data_type, arr.length)
    else
        raise(Failure("not_defined_id"))

(* check if s is a valid array index *)
let valid_index env s = 
    try int_of_string s
        with (Failure "int_of_string") -> -1

(* get the type of element 
 * if it is an array type, return it real type
 *)
let type_of_element env = function
    Nid(s) -> 
        let typ, _ = type_of_id env s in typ
    |Arrayid(s1, s2) -> 
            let typ, len = type_of_id env s1 in
            let index = valid_index env s2 in
            if index > (len-1) then
               raise(Failure("array index is out bound"))
            else 
            (match typ with 
                | VarArr -> Var
                | VectorArr -> Vector
                | MatrixArr -> Matrix
                | VecSpaceArr -> VecSpace
                | InSpaceArr -> InSpace
                | AffSpaceArr -> AffSpace
                | _ -> raise(Failure("wrong array type")) )

(* input: a list of expressions
 *        target type
 *        translate environment
 * output: true or false
 * check if a list of expression have same target type *)
let rec check_list env typ = function
    [] -> true
    | hd::tl ->
        let typ' = type_of env hd in
            if typ' <> typ then
                false
            else check_list env typ tl
    
(* find the type of a expression 
 * and do checking during get the type
 * *)
and type_of env  = function
    Literal(s) -> Var
    | Id(el) ->
            type_of_element env el
    | Binop(e1, op, e2) -> 
        (match op with
        | Add ->  (match (type_of env e1, type_of env e2) with
                        ( Var, Var ) -> Var
                        | ( Vector, Vector) -> Vector
                        | ( Matrix, Matrix) -> Matrix 
                        | ( VecSpace, VecSpace) -> VecSpace
                        | _ -> raise(Failure("in add(sub) two operands don't have same type")))
        
        | Sub  -> (match (type_of env e1, type_of env e2) with
                        ( Var, Var ) -> Var
                        | ( Vector, Vector) -> Vector
                        | ( Matrix, Matrix) -> Matrix 
                        | _ -> raise(Failure("in add(sub) two operands don't have same type")))
        | Mult | Div -> (match (type_of env e1, type_of env e2) with
                        ( Var, Var ) -> Var
                        | ( Matrix, Matrix) -> Matrix
                        | _ -> raise(Failure("in * fail in type checking")))
               
        | Add_Dot | Sub_Dot | Mult_Dot | Div_Dot ->
                        (match (type_of env e1, type_of env e2) with
                        ( Var, Vector ) -> Vector
                        | ( Vector, Var ) -> Vector
                        | ( Var, Matrix ) -> Matrix
                        | ( Matrix, Var ) -> Matrix
                        | ( Matrix, Matrix) -> Matrix
                        | _ -> raise(Failure("in +., -. fail in type checking")))
        | Equal | Neq | Less | Leq
        | Greater | Geq | And | Or -> 
                        ( match (type_of env e1, type_of env e2) with
                        ( Var, Var ) -> Var
                        | _ -> raise(Failure("in comparasion fail in type checking")))
        )
    | Assign(el, e) ->
            let el_type = type_of_element env el in
            let expr_type = type_of env e in
                if el_type = expr_type then
                    el_type
                else
                    raise(Failure("in assign fail in type checking"))
    | AssignArr(el, eList) ->
            let el_type = type_of_element env el in
            if check_list env el_type eList then
                el_type
            else
                raise(Failure("in assign array fail in type checking"))
    | Call(fid, eList) -> 
        let fdecl = 
            if is_func fid env then
                find_func fid env
            else 
                raise(Failure("in call not defined function"))
        in
        let rec check_two_lists env list1 list2 fdecl= 
            match list1, list2 with
                [],[] -> fdecl.ret_type
                | hd1::tl1, [] -> raise(Failure("in call fail in type checking(not same length)"))
                | [], hd2::tl2 -> raise(Failure("in call fail in type checking(not same length)")) 
                | hd1::tl1, hd2::tl2 -> 
                    let typ1 = 
                        (match hd1 with
                            Lvardecl(var) -> var.data_type
                            | Larraydecl(arr) -> arr.data_type
                        )
                    in 
                    let typ2 = type_of env hd2 in
                    if typ1 <> typ2 then
                        raise(Failure("in call fail in type checking(not match)"))
                    else
                        check_two_lists env tl1 tl2 fdecl
        in
        check_two_lists env fdecl.params eList fdecl
    | Callbuiltin(f, el) ->
            (match f with
                | Sqrt | Ceil | Floor ->
                        if (List.length el) <> 1 then
                           raise(Failure("wrong arguments in builtin functions(type checking)"))
                        else
                            let typ = type_of env (List.hd el) in
                            if typ <> Var then
                               raise(Failure("in builtin fail in type checking"))
                            else
                               Var 
                | Dim -> 
                        if (List.length el) <> 1 then
                            raise(Failure("wrong arguments in builtin functions(type checking)"))
                        else
                            let typ = type_of env (List.hd el) in
                            if typ <> Var && typ <> Vector && typ <> VecSpace && typ <> AffSpace && typ <> InSpace then
                                raise(Failure("in builtin fail in type checking"))
                            else
                                Var
                | Size ->
                        if (List.length el) <> 1 then
                            raise(Failure("wrong arguments in builtin functions(type checking)"))
                        else
                            let typ = type_of env (List.hd el) in
                            if typ <> Matrix then
                                raise(Failure("in builtin fail in type checking"))
                            else
                                VarArr
                | Basis ->
                        if (List.length el) <> 1 then
                            raise(Failure("wrong arguments in builtin functions(type checking)"))
                        else
                            let typ = type_of env (List.hd el) in
                            if typ <> VecSpace then
                                raise(Failure("in builtin fail in type checking"))
                            else
                                Var
                | Image ->
                        if (List.length el) <> 1 then
                            raise(Failure("wrong arguments in builtin functions(type checking)"))
                        else
                            let typ = type_of env (List.hd el) in
                            if typ <> Matrix then
                                raise(Failure("in builtin fail in type checking"))
                            else
                                VecSpace
                | Rank | Trace ->
                        if (List.length el) <> 1 then
                            raise(Failure("wrong arguments in builtin functions(type checking)"))
                        else
                            let typ = type_of env (List.hd el) in
                            if typ <> Matrix then
                                raise(Failure("in builtin fail in type checking"))
                            else
                                Var
                | Evalue ->
                        if (List.length el) <> 1 then
                            raise(Failure("wrong arguments in builtin functions(type checking)"))
                        else
                            let typ = type_of env (List.hd el) in
                            if typ <> Matrix then
                                raise(Failure("in builtin fail in type checking"))
                            else
                                VarArr
                | Belongs ->
                        if (List.length el) <> 2 then
                            raise(Failure("wrong arguments in builtin functions(type checking)"))
                        else
                            ( match (type_of env (List.hd el), type_of env (List.nth el 1)) with
                                (Vector, VecSpace) -> Var
                                | (Vector, AffSpace) -> Var
                                | _ -> raise(Failure("in belongs fail in type checking")))
                | LieBracket ->
                        if (List.length el) <> 2 then
                            raise(Failure("wrong arguments in builtin functions(type checking)"))
                        else
                            ( match (type_of env (List.hd el), type_of env (List.nth el 1)) with
                                ( Matrix, Matrix ) -> Matrix
                                | _ -> raise(Failure("in liebracket fail in type checking")))
                | Inpro ->
                        if (List.length el) <> 3 then
                            raise(Failure("wrong arguments in builtin functions(type checking)"))
                        else
                            ( match (type_of env (List.hd el), type_of env (List.nth el 1), type_of env (List.nth el 2)) with
                                (InSpace, Vector, Vector) -> Var
                                | _ -> raise(Failure("in inner product fail in type checking")))
                | Transpose ->
                        if (List.length el) <> 1 then
                            raise(Failure("wrong arguments in builtin functions(type checking)"))
                        else
                            ( match type_of env (List.hd el) with
                                Matrix -> Matrix
                                | _ -> raise(Failure("in transpose fail in type checking")))
                            
                | Solve ->
                        if (List.length el) <> 2 then
                            raise(Failure("wrong arguments in builtin funcitons(type checking)"))
                        else
                            ( match (type_of env (List.hd el), type_of env (List.nth el 1)) with
                                (Matrix, Vector) -> AffSpace
                                | _ -> raise(Failure("in solve fail in type checking")))
                | Action -> 
                        if (List.length el) <> 2 then
                            raise(Failure("wrong arguments in builtin functions(type checking)"))
                        else
                            ( match (type_of env (List.hd el), type_of env (List.nth el 1)) with
                                (Matrix, Vector) -> Vector
                                | _ -> raise(Failure("in action fail in type checking")))
                | Print -> 
                        if (List.length el) <> 1 then
                            raise(Failure("wrong arguments in builtin functions(type checking)"))
                        else
                            let _ = type_of env (List.hd el) in
                                Unit)
   
    | ExprValue(v) -> 
            let typ = type_of_value env v in
                typ
    | Noexpr -> Unit

(* get the type of prim value *)
and type_of_value env = function
    VValue(s) -> Var
    | VecValue(s) -> Vector
    | MatValue(s) -> Matrix
    | VecSpValue(s) -> VecSpace
    | InSpValue(e1, e2) -> InSpace
    | AffSpValue(e1, e2) -> AffSpace
    | Expression(typ, e) -> typ
    | Notknown -> Unit      

open Ast
open Translate_env

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

(* check if s is a valid array index , input is string, output is the int number of s*)
let valid_index env s = 
    try int_of_string s
        with (Failure "int_of_string") ->
            if is_local_var s env then
                let decl = find_local_var s env in
                    (match decl with
                        | Larraydecl(arr) -> raise(Failure("not valid array index"))
                        | Lvardecl(var) ->
                            if var.data_type <> Var then
                                raise(Failure("not valid array index"))
                            else
                                match var.value with
                                    | VecValue(_) | MatValue(_) 
                                    | VecSpValue(_) | InSpValue(_,_) 
                                    | AffSpValue(_,_) | Expression(_,_) | Notknown -> raise(Failure("not valid array index")) 
                                    |  VValue(s) -> 
                                        try int_of_string s with
                                        (Failure "int_of_string") -> raise(Failure("not valid array index")))
                                    
            else if is_global_var s env then
                let decl = find_global_var s env in
                    match decl with
                        | Garraydecl(arr) -> raise(Failure("not valid array index"))
                        | Gvardecl(var) ->
                            if var.data_type <> Var then
                                raise(Failure("not valid array index"))
                            else
                                match var.value with
                                    | VecValue(_) | MatValue(_) 
                                    | VecSpValue(_) | InSpValue(_,_) 
                                    | AffSpValue(_,_) | Expression(_,_) | Notknown -> raise(Failure("not valid array index")) 
                                    |  VValue(s) -> 
                                        try int_of_string s with
                                        (Failure "int_of_string") -> raise(Failure("not valid array index"))
            else
                raise(Failure("not valid array index"))
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

let rec check_list env typ = function
    [] -> true
    | hd::tl ->
        let typ' = type_of env hd in
            if typ' <> typ then
                false
            else check_list env typ tl
    
(* find the type of a  expression *)
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
    | Assign(id, e) ->
        let id_type,_ = type_of_id env id in
        let expr_type = type_of env e in
            if id_type = expr_type then
                id_type
            else
                raise(Failure("in assign fail in type checking"))
    | AssignArr(id, eList) ->
        let id_type,_ = type_of_id env id in
        if check_list env id_type eList then
           id_type
        else
           raise(Failure("in assign array fail in type checking"))
    | Call(fid, eList) -> 
        let fdecl = 
            if is_func fid env then
                find_func fid env
            else 
                raise(Failure("in call not defined function"))
        in
        let rec check_two_lists env list1 list2 = 
            (*let length1 = List.length list1 in
            let length2 = List.length list2 in
            if length1 <> length2 then
                raise(Failure("in call fail in type checking(" ^ string_of_int length1 ^"and " ^ string_of_int length2 ^ "not in same length"))
            else*)
            match list1, list2 with
                [],[] -> Unit
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
                        check_two_lists env tl1 tl2
        in
        check_two_lists env fdecl.params eList
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
                            if typ <> Var && typ <> VecSpace && typ <> AffSpace then
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
                | Print -> Unit)
   (* | Vsconst(eList) ->
            if check_list env Vector eList then
               VecSpace
            else if (List.length eList == 1) then
                if check_list env VectorArr eList then
                    VecSpace
                else
                    raise(Failure("in vsconst fail in type checking"))
            else
               raise(Failure("in vsconst fail in type checking"))
    *)
    | ExprValue(v) -> 
            let typ = type_of_value env v in
                typ
    | Noexpr -> Unit

and type_of_value env = function
    VValue(s) -> Var
    | VecValue(s) -> Vector
    | MatValue(s) -> Matrix
    | VecSpValue(s) -> VecSpace
    | InSpValue(e1, e2) -> InSpace
    | AffSpValue(e1, e2) -> AffSpace
    | Expression(typ, e) -> typ
    | Notknown -> Unit      

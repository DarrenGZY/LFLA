open Ast

module StringMap = Map.Make(String)

let type_of_id env s = 
    let global_vars, global_funcs, local_vars = env in
    if StringMap.mem s local_vars then
        let decl = StringMap.find s local_vars in
            match decl with
                Lvardecl(var) -> var.data_type
                | Larraydecl(arr) -> arr.data_type
    else if StringMap.mem s global_vars then
        let decl = StringMap.find s global_vars in
            match decl with
                Gvardecl(var) -> var.data_type
                | Garraydecl(arr) -> arr.data_type
    else
        raise(Failure("not find when type checking"))



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
        (match el with
            Nid(s) -> type_of_id env s
            | Arrayid(s1, s2) -> type_of_id env s1)
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
                        | _ -> raise(Failure("in +., -. fail in type checking")))
        | Equal | Neq | Less | Leq
        | Greater | Geq | And | Or -> 
                        ( match (type_of env e1, type_of env e2) with
                        ( Var, Var ) -> Var
                        | _ -> raise(Failure("in comparasion fail in type checking")))
        )
    | Belongs(e1, e2) ->
        ( match (type_of env e1, type_of env e2) with
            (Vector, VecSpace) -> Var
            | _ -> raise(Failure("in belongs fail in type checking")))
    | LieBracket(e1, e2) ->
        ( match (type_of env e1, type_of env e2) with
            ( Matrix, Matrix ) -> Matrix
            | _ -> raise(Failure("in liebracket fail in type checking")))
    | Inpro(id, e1, e2) ->
        ( match (type_of_id env id, type_of env e1, type_of env e2) with
            (InSpace, Vector, Vector) -> Var
            | _ -> raise(Failure("in inner product fail in type checking")))
    | Transpose(e) ->
        ( match type_of env e with
            Matrix -> Matrix
            | _ -> raise(Failure("in transpose fail in type checking")))
    | Assign(id, e) ->
        let id_type = type_of_id env id in
        let expr_type = type_of env e in
            if id_type = expr_type then
                id_type
            else
                raise(Failure("in assign fail in type checking"))
    | AssignArr(id, eList) ->
        let id_type = type_of_id env id in
        if check_list env id_type eList then
           id_type
        else
           raise(Failure("in assign array fail in type checking"))
    | Call(fid, eList) -> 
        let (_, global_funcs, _) = env in
        let fdecl = 
            if StringMap.mem fid global_funcs then
                StringMap.find fid global_funcs
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
    | Builtin(el, f) ->
            let typ = (match el with
                Nid(s) -> type_of_id env s
                | Arrayid(s1, s2) -> type_of_id env s1) 
            in
            (match f with
                | Dim -> 
                        if typ <> Var && typ <> VecSpace && typ <> AffSpace then
                            raise(Failure("in builtin fail in type checking"))
                        else
                            Var
                | Size ->
                        if typ <> Matrix then
                            raise(Failure("in builtin fail in type checking"))
                        else
                            Var
                | Basis ->
                        if typ <> VecSpace then
                            raise(Failure("in builtin fail in type checking"))
                        else
                            Var
            )
    | Print(e) -> Unit
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

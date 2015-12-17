open Past

module StringMap = Map.Make(String)

let string_of_elem = function
    | P_nid(s) -> s
    | P_arrayid(s1, s2) -> s1 ^ "[" ^ s2 ^ "]"

let rec string_of_expr = function (*TODO: Add symbol table as argument*)
    P_literal(l) -> l
    | P_id(el) -> string_of_elem el
    | P_transpose(e) -> "np.transpose(" ^ string_of_expr e ^ ")"
    | P_binop(e1, o, e2) ->
        string_of_expr e1 ^ " " ^
        (match o with
        Padd -> "+" | Psub -> "-" | Pmult -> "*" | Pdiv -> "/"
        | Padd_Dot -> "+" | Psub_Dot -> "-" | Pmult_Dot -> "*"    
        | Pdiv_Dot -> "/" | Pequal -> "==" | Pneq -> "!="
        | Pless -> "<" | Pleq -> "<=" | Pgreater -> ">" | Pgeq -> ">="
        | Pand -> "&&" | Por -> "||"   ) ^ " " ^
        string_of_expr e2  
    (* builtin functions *) 
    | P_belongs(e1, e2) -> string_of_expr e2 ^ ".belongs(" ^ string_of_expr e1 ^ ")"
    | P_lieBracket(e1, e2) -> "liebracket(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
    | P_inpro(id, e1, e2) -> string_of_expr id ^ ".product(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")" 
    | P_assign(v, e) -> v ^ " = " ^ string_of_expr e
    | P_assignArr(v, e) -> v ^ " = [" ^ String.concat "," (List.map string_of_expr e) ^ "]"
    | P_call(f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    | P_ceil(e) -> "ceil(" ^ string_of_expr e^ ")"
    | P_floor(e) -> "floor(" ^ string_of_expr e ^ ")"
    | P_sqrt(e) -> "sqrt(" ^ string_of_expr e ^ ")"
    | P_solve(e1, e2) -> "solve(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
    | P_dim(typ, e) -> 
            (match typ with
                P_vector -> string_of_expr e ^ ".size"
                | P_vecSpace | P_inSpace | P_affSpace -> string_of_expr e ^ ".dim()"
                | _ -> "wrong type")
    | P_size(e) -> string_of_expr e ^ ".shape"
    | P_basis(e) -> string_of_expr e ^ ".basis()"
    | P_trace(e) -> "trace(" ^ string_of_expr e ^ ")"
    | P_rank(e) -> "rank(" ^ string_of_expr e ^ ")"
    | P_image(e) -> "image(" ^ string_of_expr e ^ ")"
    | P_evalue(e) -> "eigen(" ^ string_of_expr e ^ ")"
    | P_print(e) -> "print(" ^ string_of_expr e ^ ")"
    | P_exprValue(v) -> string_of_prim_value v
    | P_noexpr -> ""
    | P_matrixMul(e1, e2) -> "np.multiply(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"

and string_of_prim_value = function
    P_Value(s) -> s
    | P_VecValue(s) -> "np.array([" ^ String.concat "," s ^ "])"
    | P_MatValue(s) -> "np.matrix((" ^ String.concat "," (List.map (fun s -> "(" ^ s ^ ")") (List.map (String.concat ",") s)) ^ "))"
    | P_VecSpValue(eList) -> "VecSpace([" ^ String.concat "," (List.map string_of_expr eList) ^ "])"    
    | P_VecSpValueArr(eList) -> "VecSpace(" ^ String.concat "," (List.map string_of_expr eList) ^ ")" 
    | P_InSpValue(e1, e2) -> "InSpace(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"            
    | P_AffSpValue(e1, e2) -> "AffSpace(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"      
    | P_Expression(e) -> string_of_expr e
    | P_Notknown -> ""   

let string_of_prim_type = function
    P_var -> "0"
    | P_vector -> "np.array([])"
    | P_matrix -> "np.matrix(([[]]))"
    | P_vecSpace -> "VecSpace()"
    | P_inSpace -> "InSpace()"
    | P_affSpace -> "AffSpace()"
    | P_varArr -> "[]"
    | P_vectorArr -> "[]"
    | P_matrixArr -> "[]"
    | P_vecSpaceArr -> "[]"
    | P_inSpaceArr -> "[]"
    | P_affSpaceArr -> "[]"
    | P_unit -> ""

let string_of_normal_decl num_ident decl = 
    let spaces = 4 in
    match decl with
    P_Vardecl(v) -> (String.make (num_ident*spaces) ' ') ^ v.p_vname ^ "=" ^ 
        ( match v.p_value with
            P_Notknown -> string_of_prim_type v.p_data_type
            | _ -> string_of_prim_value v.p_value
        ) ^ "\n"
    | P_Arraydecl(a) -> 
            (String.make (num_ident*spaces) ' ') ^ a.p_aname ^ "=[" ^ String.concat "," (List.map string_of_expr a.p_elements) ^ "]\n"

(* num_ident indicates the number of tabs at the begin of the statement, each tab is three spaces *)
let rec string_of_stmt num_ident stmt = 
    let spaces = 4 in 
    match stmt with 
    P_block(stmts) ->
        "\n" ^ (String.make (num_ident*spaces) ' ') ^ String.concat "" (List.map (string_of_stmt (num_ident+1)) stmts) ^ "\n"
    | P_expr(expr) -> (String.make (num_ident*spaces) ' ') ^ string_of_expr expr ^ "\n";
    | P_return(expr) -> (String.make (num_ident*spaces) ' ') ^ "return " ^ string_of_expr expr ^ "\n";
    | P_if(e, s, []) -> (String.make (num_ident*spaces) ' ') ^ "if " ^ string_of_expr e ^ ":\n" 
        ^ String.concat "" (List.map (string_of_stmt (num_ident+1)) s)
    | P_if(e, s1, s2) -> (String.make (num_ident*spaces) ' ') ^ "if " ^ string_of_expr e ^ ":\n"
        ^ String.concat "" (List.map (string_of_stmt (num_ident+1)) s1) ^ "\n" 
        ^ (String.make (num_ident*spaces) ' ') ^ "else:\n" ^ String.concat "" (List.map (string_of_stmt (num_ident+1)) s2) ^ "\n"
    | P_for(l, a1, a2, s) ->
        (String.make (num_ident*spaces) ' ') ^ "for " ^ l ^ " in range(" ^ string_of_expr a1 ^ ", " ^  string_of_expr a2  ^ ") :\n" 
        ^ String.concat "" (List.map (string_of_stmt (num_ident+1)) s) ^ "\n"
    | P_while(e, s) -> (String.make (num_ident*spaces) ' ') ^ "while " ^ string_of_expr e ^ ": \n" 
        ^ String.concat "" (List.map (string_of_stmt (num_ident+1)) s) ^ "\n"
    | P_continue -> (String.make (num_ident*spaces) ' ') ^ "continue "
    | P_break -> (String.make (num_ident*spaces) ' ') ^ "break "
    | P_decl(d) -> string_of_normal_decl num_ident d



let string_of_params = function
    P_Vardecl(v) -> v.p_vname
    | P_Arraydecl(a) -> a.p_aname


let string_of_func_stmt = function
    P_Local(l) -> string_of_normal_decl 1 l
    | P_Body(s) -> string_of_stmt 1 s

let string_of_func_decl fdecl =
    "def " ^ fdecl.p_fname ^ "(" ^ String.concat "," (List.map string_of_params fdecl.p_params) ^  
    ") :\n" ^ String.concat "" (List.map (string_of_stmt 1) fdecl.p_body) ^ "\n"

let string_of_program_stmt = function
    P_Variable(v) -> string_of_normal_decl 0 v
    | P_Function(f) -> string_of_func_decl f

(* input is ast tree(normal_decl list * func_decl list), output is a python file *)
let compile program = 
    String.concat "" (List.map string_of_program_stmt program) ^
    "main()" 
    
     

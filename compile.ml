open Ast
open Past
open Parser

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
    | P_belongs(e1, e2) -> string_of_expr e2 ^ ".belongs(" ^ string_of_expr e1 ^ ")"
    | P_lieBracket(e1, e2) -> string_of_expr e1 ^ ".liebracket(" ^ string_of_expr e2 ^ ")"
    | P_inpro(id, e1, e2) -> string_of_expr e1 ^ ".innerproduct(" ^ string_of_expr e2 ^ ")" 
    | P_assign(v, e) -> v ^ " = " ^ string_of_expr e
    | P_assignArr(v, e) -> v ^ " = [" ^ String.concat "," (List.map string_of_expr e) ^ "]"
    | P_call(f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    | P_builtin(el, s) ->
        ( match s with
            P_dim -> string_of_elem el ^ ".size"
            | P_size -> string_of_elem el ^ ".shape"
            | P_vsconst -> "VecSpace([" ^ string_of_elem el ^ "])"
        )
    | P_print(e) -> "print(" ^ string_of_expr e ^ ")"
    | P_noexpr -> ""
    
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

let string_of_prim_value = function
    P_Value(s) -> s
    | P_VecValue(s) -> "np.array([" ^ String.concat "," s ^ "])"
    | P_MatValue(s) -> "np.matrix((" ^ String.concat "," (List.map (fun s -> "(" ^ s ^ ")") (List.map (String.concat ",") s)) ^ "))"
    | P_VecSpValue(s) -> "VecSpace([" ^ String.concat "," s ^ "])"    
    | P_InSpValue(s1, s2) -> "InSpace(" ^ s1 ^ "," ^ s2 ^ ")"            
    | P_AffSpValue(s1, s2) -> "AffSpace(" ^ s1 ^ "," ^ s2 ^ ")"      
    | P_Expression(e) -> string_of_expr e
    | P_Notknown -> ""


let string_of_prim_type = function
    P_var -> "0"
    | P_vector -> "np.array([])"
    | P_matrix -> "np.matrix(([[]]))"
    | P_vecSpace -> "VecSpace()"
    | P_inSpace -> "InSpace()"
    | P_affSpace -> "AffSpace()"

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

let string_of_params = function
    P_Vardecl(v) -> v.p_vname
    | P_Arraydecl(a) -> a.p_aname


let string_of_func_stmt = function
    P_Local(l) -> string_of_normal_decl 1 l
    | P_Body(s) -> string_of_stmt 1 s

let string_of_func_decl fdecl =
    "def " ^ fdecl.p_fname ^ "(" ^ String.concat "," (List.map string_of_params fdecl.p_params) ^  
    ") :\n" ^ String.concat "" (List.map string_of_func_stmt fdecl.p_body) ^ "\n"

let string_of_program_stmt = function
    P_Variable(v) -> string_of_normal_decl 0 v
    | P_Function(f) -> string_of_func_decl f

(* input is ast tree(normal_decl list * func_decl list), output is a python file *)
let compile program = 
    let getName = function
        P_Variable(v) -> 
            (match v with
                P_Vardecl(pV) -> pV.p_vname
                | P_Arraydecl(pA) -> pA.p_aname
            )
        | P_Function(f) -> f.p_fname
    in
    let global_table = 
        List.fold_left (fun m decl -> StringMap.add (getName decl) 0 m) StringMap.empty program
    in
    if StringMap.mem "main" global_table then
        String.concat "" (List.map string_of_program_stmt program) ^
        "main()" 
    else
        raise (Failure("no main function"))
    
     

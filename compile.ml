open Ast
open Parser

module StringMap = Map.Make(String)

let string_of_elem = function
    | Nid(s) -> s
    | Arrayid(s1, s2) -> s1 ^ "[" ^ s2 ^ "]"

let rec string_of_expr = function
    Literal(l) -> l
    | Id(el) -> string_of_elem el
    | Transpose(e) -> "np.transpose(" ^ string_of_expr e ^ ")"
    | Binop(e1, o, e2) ->
        string_of_expr e1 ^ " " ^
        (match o with
        Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
        | Add_Dot -> "+" | Sub_Dot -> "-" | Mult_Dot -> "*"    
        | Div_Dot -> "/" | Equal -> "==" | Neq -> "!="
        | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
        | And -> "&&" | Or -> "||"   ) ^ " " ^
        string_of_expr e2   
    | Belongs(e1, e2) -> string_of_expr e2 ^ ".belongs(" ^ string_of_expr e1 ^ ")"
    | LieBracket(id, e1, e2) -> string_of_expr e1 ^ ".liebracket(" ^ string_of_expr e2 ^ ")"
    | Inpro(e1, e2) -> string_of_expr e1 ^ ".innerproduct(" ^ string_of_expr e2 ^ ")" 
    | Assign(v, e) -> v ^ " = " ^ string_of_expr e
    | Call(f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    | Builtin(el, s) ->
        ( match s with
            Dim -> string_of_elem el ^ ".size"
            | Size -> string_of_elem el ^ ".shape"
            | Vsconst -> "VecSpace([" ^ string_of_elem el ^ "])"
        )
    | Noexpr -> ""
    
(* num_ident indicates the number of tabs at the begin of the statement, each tab is three spaces *)
let rec string_of_stmt num_ident = function
    Block(stmts) ->
        "\n" ^ (String.make (num_ident*3) ' ') ^ String.concat "" (List.map (string_of_stmt (num_ident+1)) stmts) ^ "\n"
    | Expr(expr) -> (String.make (num_ident*3) ' ') ^ string_of_expr expr ^ "\n";
    | Return(expr) -> (String.make (num_ident*3) ' ') ^ "return " ^ string_of_expr expr ^ "\n";
    | If(e, s, []) -> (String.make (num_ident*3) ' ') ^ "if " ^ string_of_expr e ^ ":\n" 
        ^ String.concat "" (List.map (string_of_stmt (num_ident+1)) s)
    | If(e, s1, s2) -> (String.make (num_ident*3) ' ') ^ "if " ^ string_of_expr e ^ ":\n"
        ^ String.concat "" (List.map (string_of_stmt (num_ident+1)) s1) ^ "\n" 
        ^ (String.make (num_ident*3) ' ') ^ "else:\n" ^ String.concat "" (List.map (string_of_stmt (num_ident+1)) s2) ^ "\n"
    | For(l, a1, a2, s) ->
        (String.make (num_ident*3) ' ') ^ "for " ^ l ^ " in range(" ^ string_of_expr a1 ^ ", " ^  string_of_expr a2  ^ ") :\n" 
        ^ String.concat "" (List.map (string_of_stmt (num_ident+1)) s) ^ "\n"
    | While(e, s) -> (String.make (num_ident*3) ' ') ^ "while " ^ string_of_expr e ^ ": \n" 
        ^ String.concat "" (List.map (string_of_stmt (num_ident+1)) s) ^ "\n"
    | Continue -> (String.make (num_ident*3) ' ') ^ "continue "
    | Break -> (String.make (num_ident*3) ' ') ^ "break "

let string_of_prim_value = function
    VValue(s) -> s
    | VecValue(s) -> "np.array([" ^ String.concat "," s ^ "])"
    | MatValue(s) -> "np.matrix((" ^ String.concat "," (List.map (fun s -> "(" ^ s ^ ")") (List.map (String.concat ",") s)) ^ "))"
    | VecSpValue(s) -> "VecSpace([" ^ String.concat "," s ^ "])"    
    | InSpValue(s1, s2) -> "InSpace(" ^ s1 ^ "," ^ s2 ^ ")"            
    | AffSpValue(s1, s2) -> "AffSpace(" ^ s1 ^ "," ^ s2 ^ ")"      
    | Notknown -> ""


let string_of_prim_type = function
    Var -> "0"
    | Vector -> "np.array([])"
    | Matrix -> "np.matrix(([[]]))"
    | VecSpace -> "VecSpace()"
    | InSpace -> "InSpace()"
    | AffSpace -> "AffSpace()"

let string_of_normal_decl = function
    Vardecl(v) -> v.vname ^ "=" ^ 
        ( match v.value with
            Notknown -> string_of_prim_type v.data_type
            | _ -> string_of_prim_value v.value
        ) ^ "\n"
    | Arraydecl(a) -> a.aname ^ "=[" ^ String.concat "," (List.map string_of_expr a.elements) ^ "]\n"

let string_of_params = function
    Vardecl(v) -> v.vname
    | Arraydecl(a) -> a.aname

let string_of_func_decl fdecl =
    "def " ^ fdecl.fname ^ "(" ^ String.concat "," (List.map string_of_params fdecl.params) ^  
    ") :\n" ^ "   " ^ String.concat "   " (List.map string_of_normal_decl fdecl.locals) ^ 
    "\n" ^ String.concat "" (List.map (string_of_stmt 1) fdecl.body) ^ "\n"

(* input is ast tree(normal_decl list * func_decl list), output is a python file *)
let compile (normals, functions) = 
    
    let func_table = 
        List.fold_left (fun m func -> StringMap.add func.fname 0 m) StringMap.empty functions
    in
    if StringMap.mem "main" func_table then
        String.concat "" (List.map string_of_normal_decl normals) ^
        String.concat "" (List.map string_of_func_decl functions) ^
        "main()" 
    else
        raise (Failure("no main function"))
    
     

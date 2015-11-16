open Ast
open Parser

module StringMap = Map.Make(String)
let rec string_of_expr = function
    Literal(l) -> l
    | Id(s) -> s
    | Binop(e1, o, e2) ->
        string_of_expr e1 ^ " " ^
        (match o with
        Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
        | Add_Dot -> "+" | Sub_Dot -> "-" | Mult_Dot -> "*"    
        | Div_Dot -> "/" | Equal -> "==" | Neq -> "!="
        | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
        string_of_expr e2   (*TODO: missed operators: &&, ||, ', @, <<>>, [[]], etc *)
    | Assign(v, e) -> v ^ " = " ^ string_of_expr e
    | Call(f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
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
        (String.make (num_ident*3) ' ') ^ "for " ^ l ^ " in range(" ^ a1 ^ ", " ^  a2  ^ ") :\n" 
        ^ String.concat "" (List.map (string_of_stmt (num_ident+1)) s) ^ "\n"
    | While(e, s) -> (String.make (num_ident*3) ' ') ^ "while " ^ string_of_expr e ^ ": \n" 
        ^ String.concat "" (List.map (string_of_stmt (num_ident+1)) s) ^ "\n"


let string_of_prim_value = function
    VValue(s) -> s
    | VecValue(s) -> "np.array([" ^ String.concat "," s ^ "])"
    | MatValue(s) -> "np.matrix((" ^ String.concat "," (List.map (fun s -> "(" ^ s ^ ")") (List.map (String.concat ",") s)) ^ "))"
    | VecSpValue(s) -> "vecspace(" ^ String.concat "," s ^ ")"      (*TODO: define vecspace in python*)
    | InSpValue(s1, s2) -> "inspace(" ^ s1 ^ "," ^ s2 ^ ")"         (*TODO: define inspace in python*)
    | AffSpValue(s1, s2) -> "affspace(" ^ s1 ^ "," ^ s2 ^ ")"       (*TODO: define affspace in python*)
    | Notknown -> ""


let string_of_normal_decl = function
    Vardecl(v) -> v.vname ^ "=" ^ string_of_prim_value v.value ^ "\n"
    | Arraydecl(a) -> a.aname ^ "\n"

let string_of_func_decl fdecl =
    "def " ^ fdecl.fname ^ "() :\n" ^ "   " ^ String.concat "   " (List.map string_of_normal_decl fdecl.locals) ^ 
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
    
     
    (*
    let pyFile = Out_channel.create "test.py" in
    fprintf pyFile "%s\n" "import numpy as np";
    
    let rec print_func funcs = 
        match funcs with
        | [] -> fprintf pyFile "%s\n" "#end"
        | hd::tl -> fprintf pyFile "%s\n" hd.fname; print_func tl
    in

    print_func functions;
    Out_channel.close pyFile
    *)




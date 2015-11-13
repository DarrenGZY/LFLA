open Ast
open Parser


let rec string_of_expr = function
    Literal(l) -> l
    | Id(s) -> s
    | Binop(e1, o, e2) ->
        string_of_expr e1 ^ " " ^
        (match o with
        Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
        | Equal -> "==" | Neq -> "!="
        | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
        string_of_expr e2
    | Assign(v, e) -> v ^ " = " ^ string_of_expr e
    | Call(f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    | Noexpr -> ""


let rec string_of_stmt = function
    Block(stmts) ->
        "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
    | Expr(expr) -> string_of_expr expr ^ "\n";
    | Return(expr) -> "return " ^ string_of_expr expr ^ "\n";
    | If(e, s, []) -> "if (" ^ string_of_expr e ^ ")\n" ^ String.concat "" (List.map string_of_stmt s)
    | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
        String.concat "" (List.map string_of_stmt s1) ^ "else\n" ^ String.concat "" (List.map string_of_stmt s2)
    | For(a1, a2, s) ->
        "for (" ^ string_of_int a1  ^ " ; " ^ string_of_int a2  ^ ") " ^ String.concat "" (List.map string_of_stmt s)
    | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ String.concat "" (List.map string_of_stmt s)


let string_of_prim_value = function
    VValue(s) -> s
    | VecValue(s) -> "np.array([" ^ String.concat "," s ^ "])"
    | MatValue(s) -> "np.matrix((" ^ String.concat "," (List.map (fun s -> "(" ^ s ^ ")") (List.map (String.concat ",") s)) ^ "))"
    | VecSpValue(s) -> String.concat "," s
    | InSpValue(s1, s2) -> s1 ^ "," ^ s2
    | AffSpValue(s1, s2) -> s1 ^ "," ^ s2
    | Notknown -> ""


let string_of_normal_decl = function
    Vardecl(v) -> v.vname ^ "=" ^ string_of_prim_value v.value ^ "\n"
    | Arraydecl(a) -> a.aname ^ "\n"

let string_of_func_decl fdecl =
    "def " ^ fdecl.fname ^ "() :\n" ^ "   " ^ String.concat "   " (List.map string_of_normal_decl fdecl.locals) ^ 
    "\n" ^ "   " ^  String.concat "   " (List.map string_of_stmt fdecl.body) ^ "\n"

(* input is ast tree(normal_decl list * func_decl list), output is a python file *)
let compile (normals, functions) = 
    String.concat "" (List.map string_of_normal_decl normals) ^
    String.concat "" (List.map string_of_func_decl functions) ^
    "main()" 
    
     
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




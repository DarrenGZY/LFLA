open Ast

module StringMap = Map.Make(String)

type symbol_table = {
    parent : symbol_table option;
    vars : lNormal_decl StringMap.t;
}

type translate_env = {
    scope : symbol_table;

    global_vars : gNormal_decl StringMap.t;

    global_funcs : func_decl StringMap.t;

    return_type : prim_type;

    in_while : bool;

    in_for : bool;
      
}

(* check if it is a function name *)
let is_func fname env =  StringMap.mem fname env.global_funcs
(* find and return function declaration *)
let find_func fname env = StringMap.find fname env.global_funcs
(* check if it is a global variable *)
let is_global_var vname env = StringMap.mem vname env.global_vars
(* find and return variable declaration *)
let find_global_var vname env = StringMap.find vname env.global_vars
(* check if it is a variable defined *)
let rec is_scope_var vname scope = 
    if StringMap.mem vname scope.vars then
       true
    else
       (match (scope.parent) with
        Some(parent) -> is_scope_var vname parent
        | None -> false)

let is_local_var vname env = 
    is_scope_var vname env.scope

let rec find_scope_var vname scope =
    try StringMap.find vname scope.vars 
    with Not_found ->
        match (scope.parent) with
        Some(parent) -> find_scope_var vname parent
        | None -> raise Not_found

let find_local_var vname env = 
    find_scope_var vname env.scope

let is_defined_var vname env =
    if is_local_var vname env || is_global_var vname env then
        true
    else
        false

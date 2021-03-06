open Ast

module StringMap = Map.Make(String)

(* symbol table used for scope
 * parent: parent scope
 * vars: local variables table
 * *)
type symbol_table = {
    parent : symbol_table option;
    vars : lNormal_decl StringMap.t;
}

(* translation environment
 * scope: used for scope rule check
 * gloval_vars: global variable table
 * global_funcs: defined function table
 * in_while: if it is in a while loop
 * in_for: if it is in a for loop
 * *)
type translate_env = {
    scope : symbol_table;

    global_vars : gNormal_decl StringMap.t;

    global_funcs : func_decl StringMap.t;

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
(* check if it is a local variable *)
let is_local_var vname env = 
    is_scope_var vname env.scope
(* find the variable declaration *)
let rec find_scope_var vname scope =
    try StringMap.find vname scope.vars 
    with Not_found ->
        match (scope.parent) with
        Some(parent) -> find_scope_var vname parent
        | None -> raise Not_found
(* find the variable declaration *)
let find_local_var vname env = 
    find_scope_var vname env.scope
(* check if the variable is defined *)
let is_defined_var vname env =
    if is_local_var vname env || is_global_var vname env then
        true
    else
        false
(* check if the element if defined *)
let is_defined_element el env =
    match el with 
        Nid(s) -> is_defined_var s env
        |Arrayid(s1, s2) -> is_defined_var s1 env 

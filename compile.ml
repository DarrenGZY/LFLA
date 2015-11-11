open Ast
open Parser
open Core.Std

(* input is ast tree(normal_decl list * func_decl list), output is a python file *)
let compile (normals, functions) = 
   
    let pyFile = Out_channel.create "test.py" in
    fprintf pyFile "%s\n" "import numpy as np";
    
    
    let rec print_func funcs = 
        match funcs with
        | [] -> fprintf pyFile "%s\n" "#end"
        | hd::tl -> fprintf pyFile "%s\n" hd.fname; print_func tl
    in

    print_func functions;
    Out_channel.close pyFile 

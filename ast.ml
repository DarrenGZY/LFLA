type op = Add | Sub | Mult | Div | Add_Dot | Sub_Dot | Mult_Dot 
        | Div_Dot | Equal | Neq | Less | Leq | Greater | Geq
        | And | Or  

(* store var type value *)
type var_value = 
    Int of int
  | Float of float 

(* store vector type value *)
type vec_value = 
    var_value list

(* store matrix type value *)
type mat_value = 
    var_value list list

(* store vector space type value *)
type vecspace_value =
    vec_value list




type elem = 
  | Nid of string  (* normal identifier *)
  | Arrayid of string * string (* array identifier *)

type builtin_func = 
    Dim
  | Size
 (* | Vsconst *)
  | Basis

type prim_type = 
    Var
  | Vector
  | Matrix
  | VecSpace
  | InSpace
  | AffSpace
  | VarArr
  | VectorArr
  | MatrixArr
  | VecSpaceArr
  | InSpaceArr
  | AffSpaceArr
  | Unit


type expr =
    Literal of string
  | Id of elem
  | Binop of expr * op * expr
  | Belongs of expr * expr
  | LieBracket of expr * expr
  | Inpro of string * expr * expr
  | Transpose of expr
  | Assign of string * expr
  | AssignArr of string * expr list
  | Call of string * expr list
  | Builtin of elem * builtin_func
  | Print of expr
  | ExprValue of prim_value
  | Noexpr

and prim_value = 
    VValue of string
  | VecValue of string list
  | MatValue of string list list
  | VecSpValue of expr list
  | InSpValue of expr * expr
  | AffSpValue of expr * expr
  | Expression of prim_type * expr
  | Notknown


type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt list * stmt list
  | For of string * expr * expr * stmt list
  | While of expr * stmt list
  | Continue
  | Break


type var_decl = {
    vname : string;
    value : prim_value;
    data_type : prim_type;
    pos : int;
}

type array_decl = {
    aname : string;
    elements : expr list;
    data_type : prim_type;
    length : int;
    pos : int;
}

(* combine variable declarations and array declarations *)
type gNormal_decl = 
    Gvardecl of var_decl
  | Garraydecl of array_decl

type lNormal_decl = 
    Lvardecl of var_decl
  | Larraydecl of array_decl  
(*
type func_decl = {
    fname : string;
    params : normal_decl list;
    locals : normal_decl list;
    body : stmt list;
}*)
type function_stmt =
    Local of lNormal_decl
  | Body of stmt

type func_decl = {
    fname : string;
    params : lNormal_decl list;
    body : function_stmt list;
}

type program_stmt =
    Variable of gNormal_decl
  | Function of func_decl

(*type program = normal_decl list * func_decl list*)

type program = program_stmt list


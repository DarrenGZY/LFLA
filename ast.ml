type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

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

type prim_type = 
    Var
  | Vector
  | Matrix
  | VecSpace
  | InSpace
  | AffSpace

type prim_value = 
    VValue of var_value
  | VecValue of vec_value
  | MatValue of mat_value
  | VecSpValue of string list
  | InSpValue of string * string
  | AffSpValue of string * string
  | Notknown

type expr =
    Literal of int
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
  | Expr of expr
  | Return of expr
  | If of expr * stmt list * stmt list
  | For of int * int * stmt
  | While of expr * stmt list

type var_decl = {
    vname : string;
    value : prim_value;
    data_type : prim_type;
}

type array_decl = {
    aname : string;
    value : prim_value list;
    data_type : prim_type;
    length : int;
}

(* combine variable declarations and array declarations *)
type normal_decl = 
    Vardecl of var_decl
  | Arraydecl of array_decl

type func_decl = {
    fname : string;
    params : normal_decl list;
    locals : normal_decl list;
    body : stmt list;
}

type program = normal_decl list * func_decl list


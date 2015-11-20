type op = Add | Sub | Mult | Div | Add_Dot | Sub_Dot | Mult_Dot 
        | Div_Dot | Equal | Neq | Less | Leq | Greater | Geq
        | And | Or | LieBracket | Belongs | InnerProduct 

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
    VValue of string
  | VecValue of string list
  | MatValue of string list list
  | VecSpValue of string list
  | InSpValue of string * string
  | AffSpValue of string * string
  | Notknown

type expr =
    Literal of string
  | Id of string
  | Binop of expr * op * expr
  | Transpose of expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt list * stmt list
  | For of string * string * string * stmt list
  | While of expr * stmt list
  | Continue
  | Break

type var_decl = {
    vname : string;
    value : prim_value;
    data_type : prim_type;
}

type array_decl = {
    aname : string;
    elements : expr list;
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


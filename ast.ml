type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

(* store var type value *)
type var = 
    Int of String
  | Float of String 

	
type prim_value = 
	Var of var
  | Vector of var array
  | Matrix of var array array
  | VectorSpace of Vector list 
  | InSpace of  Vector list * Matrix
  | AffSpace  of Vecotr * VectorSpace

  
(* prime type used in variable declarations, function declarations  *)
type prim_type = 
    VAR     (* type of var *)
  | VEC		(* type of vector *)
  | MAT		(* type of matrix *)
  | VECSPA	(* type of vector space *)
  | INSPA	(* type of inner space *)
  | AFFSPA  (* type of affine space *)

type expr =
  Liter of String
  | Prim of prim_value  
  | Arra of  prim_value array	
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt list * stmt list
  | For of int * int * stmt list
  | While of expr * stmt list

type var_decl = {
    vname : string;
    data_type : prim_type;
}

type array_decl = {
    aname : string;
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


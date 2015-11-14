type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq 
		| AddDot | SubDot | MultDot | DivDot | Belongs | LieBracket | InnerProduct

type uniop = Transpose


(* prime type used in variable declarations, function declarations  *)
type prim_type = 
    VaR     (* type of var *)
  | VeC		(* type of vector *)
  | MaT		(* type of matrix *)
  | VecspA	(* type of vector space *)
  | InspA	(* type of inner space *)
  | AffspA  (* type of affine space *)

type expr =
  Liter of string
  | Vector of expr list
  | Matrix of expr list list
  | VectorSpace of expr list 
  | InSpace of  expr * expr
  | AffSpace  of expr * expr
  | Arra of  expr list	
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
  | For of string * string * stmt list
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


(* operators *)
type op = Add | Sub | Mult | Div | Add_Dot | Sub_Dot | Mult_Dot 
        | Div_Dot | Equal | Neq | Less | Leq | Greater | Geq
        | And | Or  
(* 
 * element, normal id or 
 * array id with index 
 * *)
type elem = 
  | Nid of string  (* normal identifier *)
  | Arrayid of string * string (* array identifier *)

(* builtin functions *)
type builtin_func = 
    Sqrt
  | Ceil
  | Floor
  | Dim
  | Size
  | Basis
  | Image
  | Rank
  | Trace
  | Evalue
  | Solve
  | Belongs
  | LieBracket
  | Inpro
  | Transpose
  | Print
  | Action

(* primitive types, seperate 
 * noraml types and array types 
 * *)
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

(* expressions 
 * *)
type expr =
    Literal of string
  | Id of elem
  | Binop of expr * op * expr
  | Assign of elem * expr
  | AssignArr of elem * expr list
  | Call of string * expr list
  | Callbuiltin of builtin_func * expr list
  | ExprValue of prim_value
  | Noexpr

(*  value of primitive types *)
and prim_value = 
    VValue of string
  | VecValue of string list
  | MatValue of string list list
  | VecSpValue of expr list
  | InSpValue of expr * expr
  | AffSpValue of expr * expr
  | Expression of prim_type * expr
  | Notknown

(* variable declaration
 * vname : name of variable
 * value : value of variable
 * data_type : type of variable
 * pos : position in original code(not used)
 * *)
type var_decl = {
    vname : string;
    value : prim_value;
    data_type : prim_type;
    pos : int;
}

(* array declaration
 * aname : name of array identifier
 * elements : expression list represents the elements of array
 * length : length of the array
 * data_type : type of variable
 * pos : position in original code(not used)
 * *)
type array_decl = {
    aname : string;
    elements : expr list;
    data_type : prim_type;
    mutable length : int;
    pos : int;
}

(* combine variable declarations and array declarations 
 * only represent global declaration
 * *)
type gNormal_decl = 
    Gvardecl of var_decl
  | Garraydecl of array_decl

(* combine variable declarations and array declarations 
 * only represent local declaration
 * *)
type lNormal_decl = 
    Lvardecl of var_decl
  | Larraydecl of array_decl 

(* statements *)
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt list * stmt list
  | For of string * expr * expr * stmt list
  | While of expr * stmt list
  | Continue
  | Break
  | Decl of lNormal_decl 

(* function declaration 
 * fname : name of function
 * params : list of local normal declarations
 * body : main part of function, a list of statments
 * ret_type : return type of function
 *)
type func_decl = {
    fname : string;
    params : lNormal_decl list;
    body : stmt list;
    ret_type : prim_type;
}

(* combine gloval variable declaration and funciton declaration *)
type program_stmt =
    Variable of gNormal_decl
  | Function of func_decl

type program = program_stmt list

(* input : prim_type
 * output : prim_type
 * get the real data type if it is a array type 
 * otherwise remain same
 * *)
let real_type = function
    Var -> Var
    | Vector -> Vector
    | Matrix -> Matrix
    | VecSpace -> VecSpace
    | InSpace -> InSpace
    | AffSpace -> AffSpace
    | VarArr -> Var
    | VectorArr -> Vector
    | MatrixArr -> Matrix
    | VecSpaceArr -> VecSpace
    | InSpaceArr -> InSpace
    | AffSpaceArr -> AffSpace
    | Unit -> Unit 

(* input : prim_type
 * output : prim_type
 * get the array data if it is a normal type
 * otherwise remain same
 * *)
let array_type = function
    Var -> VarArr
    | Vector -> VectorArr
    | Matrix -> MatrixArr
    | VecSpace -> VecSpaceArr
    | InSpace -> InSpaceArr
    | AffSpace -> AffSpaceArr
    | VarArr -> VarArr
    | VectorArr -> VectorArr
    | MatrixArr -> MatrixArr
    | VecSpaceArr -> VecSpaceArr
    | InSpaceArr -> InSpaceArr
    | AffSpaceArr -> AffSpaceArr
    | Unit -> Unit

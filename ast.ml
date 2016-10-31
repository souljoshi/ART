(* Abstract Syntax Tree and functions for printing it *)

(* binary operations *)
type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or  | Asn  | AddAsn | SubAsn | MultAsn | DivAsn | ModAsn 

(* unary operators *)
type uop = Neg | Not | Pos | Preinc | Predec 

(* postfix operation*)
type pop = Postinc |Postdec

(* trinary operation *)
type trop = Cond

(*type typ = Int | Char | Float | Vec | Void | Array of typ * expr | UserType of string*)


type expr =
    IntLit of int
  | CharLit of char
  | FloatLit of float
  | VecLit of (float * float)
  | Id of string
  | Vecexpr of expr * expr
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Posop of pop * expr
  | Trop of trop * expr * expr * expr
  | Call of expr * expr list (* expr list = list of arguments *)
  | Index of expr * expr (* more general than it needs to be, needs to be checked for symantec *)
  | Member of expr * string
  | Noexpr

type ss = StructType | ShapeType
(* these are the types you can use to declare an object *)
type typ = Int | Char | Float | Vec | Void | Array of typ * expr | UserType of string*ss

type initer = Exprinit of expr | Listinit of initer list | IListinit of initer list(* Incomplete List *)
              | Noinit

(* bind is for variable declaration *)
type bind = typ * string

(*you have to specify if it is passed by reference or by value*)
type pass = Value | Ref

(* methods stored as functions *)
type fbind = typ * string * pass

type ustype = Struct of string * bind list 
            | Shape of string * bind list 
(* variable declaration *)
type vdecl = typ * string * initer

type stmt =
    Block of vdecl list * stmt list 
  | Expr of expr
  | Return of expr
  | Break
  | Continue
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt 
  | ForDec of vdecl list* expr * expr * stmt
  | While of expr * stmt
  | Timeloop of string * expr * string * expr * stmt
  | Frameloop of string * expr * string * expr * stmt
  (* Builtin Statements *)
  | Drawpoint of expr * expr
  | Addshape of expr list


(* types of functions *)
type ftyp = Func | Method | Constructor

(* allows us to store all function data in one struct *)
type fdecl = {
    rettyp : typ;
    name : string;
    params : fbind list;
    body : stmt;  (* Must be Block *)
    typ : ftyp ;
    owner: string ;  (* Refers to owning struct/shape *)
  }

(* Type of program: user type, function declaration and variable declaration *)
type prog = {
    s : ustype list;
    f : fdecl  list;
    v : vdecl  list;
    }

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Asn -> "="
  | AddAsn -> "+="
  | SubAsn -> "-="
  | MultAsn -> "*="
  | DivAsn -> "/="
  | ModAsn -> "%="
  

let string_of_uop = function
    Neg -> "-"
  | Pos -> "+"
  | Not -> "!"
  | Preinc -> "++"
  | Predec -> "--"

let string_of_pop = function
    Postinc -> "++"
  | Postdec -> "--"

let strings_of_trop = function
    Cond -> (" ? ", " :")

(* Uncomment the next comment for full parenthesized *)
let rec string_of_expr (*e = "( "^ paren_of_expr e ^ " )"
and 
paren_of_expr *) = function
    IntLit(l) -> string_of_int l
  | CharLit(l) -> "'" ^ (Char.escaped l) ^ "'"
  | FloatLit(l) -> string_of_float l
  | VecLit(a,b)  -> "< " ^ (string_of_float a) ^ " , " ^ (string_of_float b) ^ " >"
  | Id(s) -> s
  | Vecexpr(e1,e2) -> " < "^ string_of_expr e1 ^ " , " ^ string_of_expr e2 ^ " >"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Posop(o, e) -> string_of_expr e ^ string_of_pop o
  | Trop (o, e1, e2, e3) -> let t = strings_of_trop o in
            string_of_expr e1 ^ fst t ^ string_of_expr e2 ^ snd t ^ string_of_expr e3
  | Call(f, el) ->
      string_of_expr f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Index(e1, e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | Member(e1, s) -> string_of_expr e1 ^ "." ^ s
  | Noexpr -> ""

let rec string_of_typ = function
    Int -> "int"
  | Char -> "char"
  | Void -> "void"
  | Float -> "double"
  | Vec  -> "vec"
  | UserType(s,StructType) -> "struct " ^ s
  | UserType(s, ShapeType) -> "shape " ^ s
  | Array(t, i) -> string_of_typ t ^ "[" ^ string_of_expr i ^ "]"
 
let string_of_bind (t,s) = 
    string_of_typ t ^" "^ s
let string_of_fbind (t,s, v) = 
    match v with 
      Value -> string_of_typ t ^" "^ s
    | Ref   -> string_of_typ t ^"& "^ s
let string_of_ustype = function
      Struct(s,l) -> "struct "^s^" {\n" 
        ^ String.concat ";\n" (List.map string_of_bind l) ^  ";\n}\n"
    | Shape(s,l) -> "shape "^s^" {\n" 
        ^ String.concat ";\n" (List.map string_of_bind l) ^  ";\n}\n"


let rec  string_of_initer = function
    Exprinit(e) -> string_of_expr e
    (* Already Reversed *)
    | Listinit(el) -> "{" ^ String.concat ", " (List.map string_of_initer (el)) ^ "}" 
    | IListinit(el) -> "{" ^ String.concat ", " (List.map string_of_initer (el)) ^ ", }" 
    | Noinit -> ""

let string_of_vdecl (t, id,i ) = 
    match i with
    Noinit -> string_of_typ t ^ " " ^ id ^";\n"
    | _ ->  string_of_typ t ^ " " ^ id ^ " = " ^ string_of_initer i ^";\n"


let rec string_of_stmt = function
    Block(decls, stmts) ->
      "{\n" ^  String.concat "" (List.map string_of_vdecl (decls)) 
      ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | Break      -> "break;\n"
  | Continue   -> "continue;\n"

  | If(e, s, Block([],[])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2

  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") \n" ^ string_of_stmt s
  | ForDec(d1, e2, e3, s) ->
      "for (" ^ String.concat "" (List.map string_of_vdecl (d1)) ^ " ; " ^ 
      string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ") \n" ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") \n" ^ string_of_stmt s

  | Timeloop(id1, e1, id2, e2, s) -> 
      "timeloop ( "^ id1 ^" = " ^ string_of_expr e1 ^ " ;" ^
       id2 ^" = "^ string_of_expr e2 ^ " )\n" ^ string_of_stmt s
  | Frameloop(id1, e1, id2, e2, s) ->
      "frameloop ( "^ id1 ^" = " ^ string_of_expr e1 ^ " ;" ^
      id2 ^" = "^ string_of_expr e2 ^ " )\n" ^ string_of_stmt s

  (* Builtin Statements *)
  | Drawpoint(e1, e2) -> "#drawpoint(" ^ string_of_expr e1 ^ " , "^
                          string_of_expr e2 ^");\n"
  | Addshape([e]) -> "#add( " ^ string_of_expr e ^ " );\n"
  | Addshape(el) ->
    "#add{" ^ String.concat ", " (List.map string_of_expr el) ^ "};\n"
 
let string_rettyp f = 
    match f.typ  with
      Constructor -> ""
    | _ -> string_of_typ f.rettyp
let string_fname f = 
    match f.typ with 
      Func -> f.name
    | _ -> f.owner^"::"^f.name
let string_of_fdecl f = 
    string_rettyp f  ^ " " ^ string_fname f ^ " ( " ^ 
    String.concat ", " (List.map string_of_fbind f.params) ^ " )\n" ^
    string_of_stmt f.body

let string_of_program p =
    String.concat "" (List.map string_of_vdecl p.v)  ^
    String.concat "" (List.map string_of_ustype p.s) ^ 
    String.concat "" (List.map string_of_fdecl p.f)  
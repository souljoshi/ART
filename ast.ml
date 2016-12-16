(* Abstract Syntax Tree and functions for printing it *)

(* binary operations *)
type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

(* Assignment operations *)
type asnop = Asn | CmpAsn of op (* Compound Assignment operator op= *)

(* unary operators *)
type uop = Neg | Not | Pos | Preinc | Predec 

(* postfix operation*)
type pop = Postinc |Postdec

(* trinary operation *)
type trop = Cond

(*type typ = Int | Char | Float | Vec | Void | Array of typ * expr | UserType of string*)

type stosh = StructType | ShapeType
(* these are the types you can use to declare an object *)
type typ = Int | Char | Float | Vec | Void | Array of typ * expr | UserType of string*stosh | String
and baseexpr =
    IntLit of int
  | CharLit of char
  | StringLit of string
  | FloatLit of float
  | VecLit of (float * float)
  | Id of string
  | Vecexpr of expr * expr
  | Binop of expr * op * expr
  | Asnop of expr * asnop * expr (* Assignment operation *)
  | Unop of uop * expr
  | Posop of pop * expr
  | Trop of trop * expr * expr * expr
  | Call of expr * expr list (* expr list = list of arguments *)
  | Index of expr * expr (* more general than it needs to be, needs to be checked for symantec *)
  | Member of expr * string
  | Noexpr

and expr = baseexpr * typ

type initer = Exprinit of expr | Listinit of initer list | Noinit

(* bind is for variable declaration *)
type bind = typ * string

(*you have to specify if it is passed by reference or by value*)
type pass = Value | Ref

(* methods stored as functions *)
type fbind = typ * string * pass

(* variable declaration *)
type vdecl = typ * string * initer

(* Context types *)
type context = PointContext | LineContext | TriangleContext

type stmt =
    Block of vdecl list * stmt list * context
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

(* types of functions *)
type ftyp = Func | Method | Constructor

(* allows us to store all function data in one struct *)
type fdecl = {
    rettyp : typ;
    fname : string;
    params : fbind list;
    locals : vdecl list;
    body : stmt list;
    typ : ftyp ;
    owner: string ;  (* Refers to owning struct/shape *)
  }

(* Acutally filled in the semantic step *)
type usrtype = {
    ss      : stosh; (* struct/shape? *)
    sname   : string;
    decls   : bind list; (* member var declarations *)
    ctor    : fdecl;     (* constructor *)
    methods : fdecl list;
  }

(* Type of program: user type, function declaration and variable declaration *)
type prog = {
    s : usrtype list;
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

let string_of_asnop = function
    Asn -> "="
  | CmpAsn o -> string_of_op o ^ "="


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

let string_of_chr =  function
  '\b' | '\t' | '\n' | '\r' as c -> Char.escaped c
  | c when Char.code(c) > 31  && Char.code(c) < 127 -> Char.escaped c
  | c -> "\\" ^ Printf.sprintf "%o" (Char.code c)


(* Uncomment the next comment for full parenthesized *)
let rec string_of_baseexpr (*e = "( "^ paren_of_expr e ^ " )"
and 
paren_of_expr *) = function
    IntLit(l) -> string_of_int l
  | CharLit(l) -> "'" ^ (string_of_chr l) ^ "'"
  | FloatLit(l) -> string_of_float l
  | StringLit(s) -> "" ^ s
  | VecLit(a,b)  -> "< " ^ (string_of_float a) ^ " , " ^ (string_of_float b) ^ " >"
  | Id(s) -> s
  | Vecexpr(e1,e2) -> " < "^ string_of_expr e1 ^ " , " ^ string_of_expr e2 ^ " >"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Asnop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_asnop o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Posop(o, e) -> string_of_expr e ^ string_of_pop o
  | Trop (o, e1, e2, e3) -> let t = strings_of_trop o in
            string_of_expr e1 ^ fst t ^ string_of_expr e2 ^ snd t ^ string_of_expr e3
  | Call(f, el) ->
      string_of_expr f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Index(e1, e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | Member(e1, s) -> string_of_expr e1 ^ "." ^ s
  | Noexpr -> ""
and string_of_expr (e,_) = string_of_baseexpr e

let rec list_of_arr = function
    Array(Array(_,_) as a , i) ->  let (t,l) = list_of_arr a in (t, i::l)
  | Array(t, i) -> (t, [i])
  | t -> (t, []) (* Syntactically Required but not used *)

let string_of_stosh = function
    StructType -> "struct"
  | ShapeType -> "shape"

let rec string_of_typ = function
    Int -> "int"
  | Char -> "char"
  | Void -> "void"
  | Float -> "double"
  | Vec  -> "vec"
  | String -> "string"
  | UserType(n,ss) -> string_of_stosh ss ^ n
  | Array(_, _) as a -> let (t,l) = list_of_arr a
     in string_of_typ t ^ String.concat "" (List.map (fun e -> "[" ^ string_of_expr e ^ "]") l)
 

let string_of_bind (t,s) = 
    string_of_typ t ^" "^ s
let string_of_fbind (t,s, v) = 
    match v with 
      Value -> string_of_typ t ^" "^ s
    | Ref   -> string_of_typ t ^"& "^ s

let rec  string_of_initer = function
    Exprinit(e) -> string_of_expr e
    (* Already Reversed *)
    | Listinit(el) -> "{" ^ String.concat ", " (List.map string_of_initer (el)) ^ "}" 
    | Noinit -> ""

let string_of_vdecl (t, id,i ) = 
    match i with
    Noinit -> string_of_typ t ^ " " ^ id ^";\n"
    | _ ->  string_of_typ t ^ " " ^ id ^ " = " ^ string_of_initer i ^";\n"


let rec string_of_stmt = function
    Block(decls, stmts, ctxt) ->
      let enclosers = function PointContext -> ("{","}") | LineContext -> ("[","]")
                          | TriangleContext -> ("<",">") in
      let opener,closer = enclosers ctxt in
      opener^"\n" ^  String.concat "" (List.map string_of_vdecl (decls)) 
      ^ String.concat "" (List.map string_of_stmt stmts) ^ closer ^ "\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | Break      -> "break;\n"
  | Continue   -> "continue;\n"

  | If(e, s, Block([],[],_)) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
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

 
let string_rettyp f = 
    match f.typ  with
      Constructor -> ""
    | _ -> string_of_typ f.rettyp
let string_fname f = 
    match f.typ with 
      Func -> f.fname
    | _ -> f.owner^"::"^f.fname
let string_of_fdecl f = 
    string_rettyp f  ^ " " ^ string_fname f ^ " ( " ^ 
    String.concat ", " (List.map string_of_fbind f.params) ^ " )\n" ^
    string_of_stmt (Block(f.locals, f.body,PointContext))

let string_of_usrtype s = string_of_stosh s.ss ^ " "^s.sname^" {\n"
        ^ String.concat ";\n" (List.map string_of_bind s.decls) ^  ";\n}\n"
        ^ String.concat "\n" (List.map string_of_fdecl (s.ctor::s.methods))

let string_of_program p =
    String.concat "" (List.map string_of_vdecl p.v)  ^
    String.concat "" (List.map string_of_usrtype p.s) ^
    String.concat "" (List.map string_of_fdecl p.f)  

let default_ctr n = { rettyp = Void; fname = n; params = []; locals = [];
                      body = [] ; typ = Constructor; owner = n }
(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or  | Asn  | AddAsn | SubAsn | MultAsn | DivAsn | ModAsn 

type uop = Neg | Not | Pos | Preinc | Predec 

type pop = Postinc |Postdec

type trop = Cond

type typ = Int | Char | Float | Vec

type expr =
    IntLit of int
  | CharLit of char
  | FloatLit of float
  | VecLit of (float * float)
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Posop of pop * expr
  | Trop of trop * expr * expr * expr
  | Call of expr * expr list
  | Index of expr * expr
  | Member of expr * string
  | Noexpr

type stmt =
    Block of stmt list (* Future Version will have declaration list *)
  | Expr of expr
  | Return of expr
  | Break
  | Continue
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt (* Future version will support decls *)
  | While of expr * stmt
  | Timeloop of string * expr * string * expr * stmt
  | Frameloop of string * expr * string * expr * stmt
  (* Builtin Statements *)
  | Drawpoint of expr
  | Addshape of expr list

 
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
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Posop(o, e) -> string_of_expr e ^ string_of_pop o
  | Trop (o, e1, e2, e3) -> let t = strings_of_trop o in
            string_of_expr e1 ^ fst t ^ string_of_expr e2 ^ snd t ^ string_of_expr e3
  | Call(f, el) ->
      string_of_expr f ^ "(" ^ String.concat ", " (List.map string_of_expr (List.rev el)) ^ ")"
  | Index(e1, e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | Member(e1, s) -> string_of_expr e1 ^ "." ^ s
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt (List.rev stmts)) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | Break      -> "break;\n"
  | Continue   -> "continue;\n"

  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2

  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") \n" ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") \n" ^ string_of_stmt s

  | Timeloop(id1, e1, id2, e2, s) -> 
      "timeloop ( "^ id1 ^" = " ^ string_of_expr e1 ^ " ;" ^
       id2 ^" = "^ string_of_expr e2 ^ " )\n" ^ string_of_stmt s
  | Frameloop(id1, e1, id2, e2, s) ->
      "frameloop ( "^ id1 ^" = " ^ string_of_expr e1 ^ " ;" ^
      id2 ^" = "^ string_of_expr e2 ^ " )\n" ^ string_of_stmt s

  (* Builtin Statements *)
  | Drawpoint(e) -> "#drawpoint(" ^ string_of_expr e ^ ");\n"
  | Addshape([e]) -> "#add( " ^ string_of_expr e ^ " );\n"
  | Addshape(el) ->
    "#add{" ^ String.concat ", " (List.map string_of_expr (List.rev el)) ^ "};\n"

let string_of_program prog =
    String.concat "" (List.map string_of_stmt (List.rev prog))
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

let rec string_of_expr = function
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

let string_of_program prog =
    String.concat ";\n" (List.map string_of_expr (List.rev prog)) ^ ";\n"
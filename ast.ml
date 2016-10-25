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
(* Ocamllex scanner for ART *)

{ open Parser }

let spc = [' ' '\t' '\r' '\n']

let oct = ['0' - '7']                         (* Octal digits *)
let dec = ['0' - '9']                         (* Decimal digits *)
let hex = dec | ['A' -'F' 'a' - 'f']          (* Hex digits *)  

let printable = [' ' -'~']                    (* All printable *)
(*let escapes = "\\n" | "\\t"  | "\\v"          (* Escaped chars *)
            |"\\b"  | "\\r"  | "\\f"
            |"\\a"  | "\\\\" | "\\?"
            |"\\'"  | "\\\"" | 
*)

let exp = 'e' ('+' | '-')? dec+               (* Floating point exponent *)
let double = '.' dec+ exp? 
            | dec+ ('.' dec* exp? | exp)      (* Floatin point Literal *)


rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }       (* Whitespace *)
| "/*"        { block_comment lexbuf }        (* Block Comments *)
| "//"        { line_comment lexbuf }         (* Line Comments *)
| '('         { LPAREN }
| ')'         { RPAREN }
| '{'         { LBRACE }
| '}'         { RBRACE }
| '['         { LBRACK }
| ']'         { RBRACK }
| '.'         { DOT }
| '?'         { QMARK }
| ':'         { COLON }
| ';'         { SEMI }
| ','         { COMMA }
| '+'         { PLUS }
| '-'         { MINUS }
| '*'         { TIMES }
| '/'         { DIVIDE }
| '%'         { MOD }
| '='         { ASSIGN }
| "+="        { PLUSASSIGN }
| "-="        { MINUSASSIGN }
| "*="        { TIMESASSIGN }
| "/="        { DIVASSIGN }
| "%="        { MODASSIGN }
| "++"        { PLUSPLUS }
| "--"        { MINUSMINUS }
| "=="        { EQ }
| "!="        { NEQ }
| '<'         { LT }
| "<="        { LEQ }
| '>'         { GT }
| ">="        { GEQ }
| "&&"        { AND }
| "||"        { OR }
| "!"         { NOT }
| "if"        { IF }
| "else"      { ELSE }
| "for"       { FOR }
| "while"     { WHILE }
| "timeloop"  { TLOOP }
| "frameloop" { FLOOP }
| "break"     { BREAK }
| "continue"  { CONTINUE }
| "return"    { RETURN }
 (* Builtin Functions *)
| "#add"       { ADDSHAPE }
| "#drawpoint" { DRAW }
(* Builtin Types *)
| "void"        { VOID }
| "int"         { INT  }
| "char"        { CHAR }
| "double"      { DOUBLE }
| "vec"         { VEC }
(* Type keywords *)
| "struct"      { STRUCT }
| "shape"       { SHAPE }

 (* Integer Literals *)

| '0' oct*  as lxm { INTLIT( int_of_string ("0o"^lxm))}
| '0' ('x' | 'X') hex* as lxm { INTLIT( int_of_string lxm)}
| ['1'-'9'] dec* as lxm { INTLIT(int_of_string lxm) } 

(* Identifier *)
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

(* Character Literals *)
| '\'' (printable as lex) '\''  { CHARLIT (lex) }
 (* More to be added soon *)

(* Double Literal *)
|  double as lex { FLOATLIT (float_of_string lex)}

(* Vector Literal *)
|  '<' spc* (double as lex1) spc*
    ',' spc* (double as lex2) spc* '>'  { VECTORLIT(float_of_string lex1, float_of_string lex2)}

| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and block_comment = parse
  "*/" { token lexbuf }
| _    { block_comment lexbuf }

and line_comment = parse
  '\n' { token lexbuf }
| _    { line_comment lexbuf }
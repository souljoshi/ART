(* Ocamllex scanner for ART *)

{ open Parser }

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
| "/*"     { block_comment lexbuf }           (* Block Comments *)
| "//"     { line_comment lexbuf }            (* Line Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACK }
| ']'      { RBRACK }
| '.'      { DOT }
| '?'      { QMARK }
| ':'      { COLON }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIV }
| '%'      { MOD }
| '='      { ASSIGN }
| "+="     { PLUSASSIGN }
| "-="     { MINUSASSIGN }
| "*="     { TIMESASSIGN }
| "/="     { DIVASSIGN }
| "%="     { MODASSIGN }
| "++"     { PLUSPLUS }
| "--"     { MINUSMINUS }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| '>'      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
 
 (* Integer Literals *)
| '0' oct*  as lxm { INTLIT( int_of_string ("0o"^lxm))}
| '0' ('x' | 'X') hex* as lxm { INTLIT( int_of_string )}
| ['1'-'9'] dec* as lxm { INTLIT(int_of_string lxm) } 

(* Identifier *)
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

(* Character Literals *)
| '\'' printable as lex '\''  { CHARLIT (lex.[0]) }
 (* More to be added soon *)

(* Double Literal *)
|  double as lex { FLOATLIT (lex)}

(* Vector Literal *)
|  '<' double as lex1 ',' double as lex2 '>'  { VECTORLIT(lex1, lex2)}

| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and block_comment = parse
  "*/" { token lexbuf }
| _    { block_comment lexbuf }

and line_comment = parse
  '\n' { token lexbuf }
| _    { line_comment lexbuf }
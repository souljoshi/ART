(* Ocamllex scanner for ART *)

{ open Parser

  (* Converts escape sequences into characters *)

  let char_esc = function
      "\\n"  -> Char.chr(0XA)
    | "\\t"  -> Char.chr(0X9)
    | "\\v"  -> Char.chr(0XB)
    |"\\b"   -> Char.chr(0X8)
    | "\\r"  -> Char.chr(0XD)
    | "\\f"  -> Char.chr(0XC)
    |"\\a"   -> Char.chr(0X7)
    | "\\\\" -> '\\'
    | "\\?"  -> '?'
    |"\\'"   -> '\''
    | "\\\"" -> '"'
    | e      -> raise (Failure("illegal escape " ^ e))

}

let spc = [' ' '\t' '\r' '\n']

let oct = ['0' - '7']                         (* Octal digits *)
let dec = ['0' - '9']                         (* Decimal digits *)
let hex = dec | ['A' -'F' 'a' - 'f']          (* Hex digits *)  

let printable = [' ' -'~']                    (* All printable *)
let escapes = "\\n" | "\\t"  | "\\v"          (* Escaped chars *)
            |"\\b"  | "\\r"  | "\\f"
            |"\\a"  | "\\\\" | "\\?"
            |"\\'"  | "\\\""
let octescp = (oct | oct oct | oct oct oct) (* Octal escapes *)
let hexescp = hex+                         (* Hex escapes *)


let exp = 'e' ('+' | '-')? dec+               (* Floating point exponent *)
let double = '.' dec+ exp? 
            | dec+ ('.' dec* exp? | exp)      (* Floatin point Literal *)


rule token = parse
  '\n'        {Lexing.new_line lexbuf;
                   token lexbuf}              (* Keep track of new lines for error printing *)
| [' ' '\t' '\r'] { token lexbuf }            (* Whitespace *)
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
| '&'         { AMPS }
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
| "::"        { DCOLON }
| "<<"        { LTLT }
| ">>"        { GTGT }
| "if"        { IF }
| "else"      { ELSE }
| "for"       { FOR }
| "while"     { WHILE }
| "timeloop"  { TLOOP }
| "frameloop" { FLOOP }
(*| "break"     { BREAK }
| "continue"  { CONTINUE }*)
| "return"    { RETURN }
(* Builtin Types *)
| "void"        { VOID }
| "int"         { INT  }
| "char"        { CHAR }
| "double"      { DOUBLE }
| "vec"         { VEC }
| "string"      {STRING}
(* Type keywords *)
| "struct"      { STRUCT }
| "shape"       { SHAPE }

 (* Integer Literals *)

| '0' oct*  as lxm { INTLIT( int_of_string ("0o"^lxm))} (* reads octal and converts to int *)
| '0' ('x' | 'X') hex* as lxm { INTLIT( int_of_string lxm)} (*reads hex and converts to int *)
| ['1'-'9'] dec* as lxm { INTLIT(int_of_string lxm) } (* reads int *)

(* Identifier *)
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

(* Character Literals *)
| '\'' (printable as lex) '\''  { CHARLIT (lex) }  (* printable within single quotes *)
| '\'' (escapes as lex) '\''    { CHARLIT (char_esc lex) }    (* escapes single quotes *)
| '\''"\\" (octescp as lex)'\'' { CHARLIT (Char.chr(int_of_string ("0o"^lex)))}
                                                    (* oct escapes *)
| '\''"\\x" (hexescp as lex)'\''{ CHARLIT (Char.chr(int_of_string ("0x"^lex)))}
                                                    (* hex escapes *)
| '\''("\\" printable as lex)'\'' { CHARLIT (char_esc lex) } (* Catch invalid escapes *)

(* Double Literal *)
|  double as lex { FLOATLIT (float_of_string lex)}

(* Hanlde strings as a sequence of character tokens *)
| '"'      { read_string (Buffer.create 2048) lexbuf }

(* Vector Literal *)
|  '<' spc* (double as lex1) spc*
    ',' spc* (double as lex2) spc* '>'  { VECTORLIT(float_of_string lex1, float_of_string lex2)}

| eof { EOF } (* end of line rule *)
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and block_comment = parse
  "*/" { token lexbuf }
|'\n'  {Lexing.new_line lexbuf; block_comment lexbuf}
| _    { block_comment lexbuf }

and line_comment = parse
  '\n' { Lexing.new_line lexbuf; token lexbuf }
| _    { line_comment lexbuf }

and read_string buf = parse

| '"'                                 { STRINGLIT (Buffer.contents buf) }
|  (printable as lex)                 { Buffer.add_char buf (lex) ; read_string buf lexbuf}                                  (* printable *)
|  (escapes as lex)                   { Buffer.add_char buf (char_esc lex); read_string buf lexbuf }                         (* escapes *)
|  "\\" (octescp as lex)              { Buffer.add_char buf (Char.chr(int_of_string ("0o"^lex))); read_string buf lexbuf}    (* oct escapes *)
|  "\\x" (hexescp as lex)             { Buffer.add_char buf (Char.chr(int_of_string ("0x"^lex))); read_string buf lexbuf}    (* hex escapes *)
| ("\\" printable as lex)             { Buffer.add_char buf (char_esc lex) ; read_string buf lexbuf}                         (* Catch invalid escapes *)  
| '\n'                                { raise (Failure ("Multiline string not supported")) }

| _ { raise (Failure ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
| eof { raise (Failure ("String is not terminated")) }
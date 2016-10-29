open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast =
  try
        Parser.program Scanner.token lexbuf
   with Parsing.Parse_error ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        print_endline ("Syntax Error: Line " ^ string_of_int line ^ " Column " ^ string_of_int cnum);
        exit 1
  in
  print_string (Ast.string_of_program ast)
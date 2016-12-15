/* Ocamlyacc parser for ART */

%{
  open Ast
  let make_dec_list s l = 
  List.map (fun (a,b) -> (s, a, b)) l ;; (* if there is a list of a,b, then s is appended infront *)
  let make_struct_dec_list s l = 
  List.map (fun a -> (s, a)) l ;;
  (* Build array of type t from the list of dimensions *)
  let build_array t = function
     [] -> t
   | [i] -> Array(t, i)
   | i::l -> List.fold_left (fun at e -> Array(at,e)) (Array(t, i)) l

  let default_ctr n = { rettyp = Void; fname = n; params = []; locals = [];
                        body = [] ; typ = Constructor; owner = n }
%}

%token VOID INT CHAR DOUBLE VEC STRING
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token DOT QMARK COLON COMMA SEMI AMPS DCOLON
%token PLUS MINUS TIMES DIVIDE MOD
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN 
%token DIVASSIGN MODASSIGN PLUSPLUS MINUSMINUS
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token RETURN IF ELSE FOR WHILE BREAK CONTINUE
%token STRUCT SHAPE
%token TLOOP FLOOP ADDSHAPE DRAW
%token <int> INTLIT
%token <char> CHARLIT
%token <float> FLOATLIT
%token <float * float> VECTORLIT
%token <string> STRINGLIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVASSIGN MODASSIGN
%right CONDITIONAL  /* ? : */
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right PRE /* prefix increment/decrement */
%right NOT NEG POS
%left INDEX CALL MEMB /* member */ POST /* postfix decrement/increment */

%start program
%type <Ast.prog> program

%%

program:
  decls EOF {{s = List.rev ($1.s); f = List.rev ($1.f);v = $1.v }}

decls:
  /* empty */ {{s = []; f = []; v = []}} /* { structs, funcs, vars } */
  | decls declaration  {{s = $1.s ; f = $1.f ; v = $1.v@$2}}
  | decls struct_or_shape_definition {{s = $2::$1.s ; f = $1.f ; v = $1.v}}
  | decls fdecl        {{s = $1.s ; f = $2::$1.f ; v = $1.v}}
  | decls mdecl        {{s = $1.s ; f = $2::$1.f ; v = $1.v}}
  /*| decls mdecl  {List.find}*/

fdecl:
  function_declarator LPAREN parameter_list RPAREN func_block
        { { rettyp = fst $1;
            fname = snd $1;
            params = List.rev $3;
            locals = fst $5;
            body = snd $5;
            typ = Func;
            owner= "" } }

function_declarator:
    vdecl_typ ID {($1, $2)}
  | VOID ID {(Void, $2)}

mdecl:
  method_declarator  LPAREN parameter_list RPAREN func_block
          { { rettyp = fst (fst $1);
            fname = snd (fst $1);
            params = List.rev $3;
            locals = fst $5;
            body = snd $5;
            typ = snd (snd $1);
            owner= fst (snd $1) } }

method_declarator:
    vdecl_typ ID DCOLON ID {($1, $4), ($2, Method)}  /* (rettyp, fname) (owner, typ)*/
  | VOID ID DCOLON ID {(Void, $4), ($2, Method)}  /* (rettyp, fname) (owner, typ)*/
  | ID DCOLON ID  {(Void, $3), ($1, Constructor)} /* Constructor */

func_block:
  /* Function Block */
    LBRACE stmt_list RBRACE                  { ([], List.rev $2) }
  | LBRACE declaration_list stmt_list RBRACE { ($2, List.rev $3) } /* Already Reversed */

parameter_list:
  /* No parameter case */  {[]}
  | parameter_declaration  {[$1]}
  | parameter_list COMMA parameter_declaration {$3::$1}

parameter_declaration:
    vdecl_typ ID      {($1, $2, Value)}
  | vdecl_typ AMPS ID {($1, $3, Ref)}

struct_or_shape:
    STRUCT          { StructType }
  | SHAPE           { ShapeType }

struct_or_shape_specifier:
    struct_or_shape ID  { UserType($2, $1)}

struct_or_shape_definition:
    struct_or_shape ID LBRACE struct_declaration_list RBRACE
    { { ss = $1 ; sname = $2; decls = $4 ; ctor = default_ctr $2; methods = []} }

struct_declaration_list:
   struct_declaration                              {$1}
 | struct_declaration_list struct_declaration      { $1 @ $2 }

struct_declaration:
  sdecl_typ struct_declarator_list SEMI  {make_struct_dec_list $1  (List.rev $2)}

struct_declarator_list:
    ID                         {[$1]}        
  | struct_declarator_list COMMA ID  { $3 :: $1}

 /* This causes conflicts
 ret_typ:
    VOID              { Void }
  | vdecl_typ         { $1 }

  */

vdecl_typ: /* Types that can be used in as func_param or variable declaration*/
    sdecl_typ         { $1 }
  | incompl_array_typ { $1 }

sdecl_typ: /* Types that can be used in declaring struct/shape members */
    basic_typ         { $1 }
  | full_array_typ    { $1 }

basic_typ:
    INT               { Int }
  | CHAR              { Char }
  | DOUBLE            { Float}
  | VEC               { Vec }
  | STRING            { String }
  | struct_or_shape_specifier {$1}

full_array_typ:
    basic_typ LBRACK expr RBRACK array_dim_list {Array((build_array $1 $5), $3)}
incompl_array_typ:
    basic_typ LBRACK RBRACK array_dim_list      {Array((build_array $1 $4),Noexpr)}

array_dim_list: /* List of array dimension declarations */
    /* Nothing */                     { [] }
  | array_dim_list LBRACK expr RBRACK { $3::$1 }


/* Declarations */
declaration_list:
  declaration                       {$1}
 | declaration_list declaration      { $1 @ $2 }

declaration:
  vdecl_typ init_declarator_list SEMI { make_dec_list $1  (List.rev $2) }

init_declarator_list: 
    init_declarator         { [$1] }
  | init_declarator_list COMMA init_declarator {$3 :: $1}

init_declarator:
    ID                      { ($1, Noinit) } /* without initialiser */
  | ID ASSIGN init          { ($1, $3)   }

init:
  expr    {Exprinit($1)}
  | LBRACE init_list RBRACE  {Listinit( List.rev $2)}
  | LBRACE init_list COMMA RBRACE {Listinit( List.rev $2 )}

init_list:
    init                  { [$1] } /* usefull for 2d arrays */
  | init_list COMMA init  { $3 :: $1}

stmt_list: /* inverted list of the statements */
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1 }
  | RETURN SEMI                             { Return Noexpr }
  | RETURN expr SEMI                        { Return $2 }
  | BREAK SEMI                              { Break }
  | CONTINUE SEMI                           { Continue }

  /* Block */
  | stmt_block                               { $1 }   /* defined in stmt_block: */

  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([],[],PointContext)) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }

  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  /* Deal with for with declaration */
  | FOR LPAREN declaration expr_opt SEMI expr_opt RPAREN stmt
     { ForDec($3, $4, $6, $8) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

  | TLOOP LPAREN ID ASSIGN expr SEMI ID ASSIGN expr RPAREN stmt
   { Timeloop($3, $5, $7, $9, $11) }
  | FLOOP LPAREN ID ASSIGN expr SEMI ID ASSIGN expr RPAREN stmt
   { Frameloop($3, $5, $7, $9, $11) }

  | DRAW LPAREN expr COMMA expr RPAREN SEMI            { Drawpoint($3, $5) }
  | ADDSHAPE LPAREN expr RPAREN SEMI        { Addshape([$3]) }
  | ADDSHAPE LBRACE expr_list RBRACE SEMI   { Addshape(List.rev $3) }

stmt_block:
  /* Block */
    LBRACE decl_list_stmt_list RBRACE     { Block(fst $2, snd $2, PointContext) }
  | LBRACK decl_list_stmt_list RBRACK     { Block(fst $2, snd $2, LineContext) }
  | LT decl_list_stmt_list GT             { Block(fst $2, snd $2, TriangleContext) }

decl_list_stmt_list:
    stmt_list                   {([], List.rev $1)} /* stmt_list needs to be reversed */
  | declaration_list stmt_list  {($1, List.rev $2)} /* declaration doesn't need reversing */


/* Optional Expression */
expr_opt:
  /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
  bexpr                     { $1 }

  /* Conditional */
  /* $1 = bexpr, $3 = expr, $5 = expr */
  |bexpr QMARK expr COLON expr %prec CONDITIONAL { Trop(Cond, $1, $3, $5) }

  /* postfix expressions */
  /* Assignment */
  |posexpr ASSIGN expr      { Asnop($1, Asn, $3) }
  |posexpr PLUSASSIGN expr  { Asnop($1, CmpAsn(Add), $3) }
  |posexpr MINUSASSIGN expr { Asnop($1, CmpAsn(Sub), $3) }
  |posexpr TIMESASSIGN expr { Asnop($1, CmpAsn(Mult), $3) }
  |posexpr DIVASSIGN expr   { Asnop($1, CmpAsn(Div), $3) }
  |posexpr MODASSIGN expr   { Asnop($1, CmpAsn(Mod), $3) }


/* all expressions other than assignment/conditional */
bexpr:
  /* Postfix expressions */
  posexpr                  {$1}

  /* unary */
  | PLUS  bexpr %prec POS        { Unop(Pos, $2) }
  | MINUS bexpr %prec NEG        { Unop(Neg, $2) }
  | NOT bexpr                    { Unop(Not, $2) }
  | PLUSPLUS   bexpr %prec PRE   { Unop(Preinc, $2)}
  | MINUSMINUS bexpr %prec PRE   { Unop(Predec, $2)}

  /* multiplicative */
  | bexpr TIMES  bexpr      { Binop($1, Mult,  $3) }
  | bexpr DIVIDE bexpr      { Binop($1, Div,   $3) }
  | bexpr MOD    bexpr      { Binop($1, Mod,   $3) }

  /* additive */
  | bexpr PLUS   bexpr      { Binop($1, Add,   $3) }
  | bexpr MINUS  bexpr      { Binop($1, Sub,   $3) }

  /* relational */
  | bexpr  LT    bexpr      { Binop($1, Less,   $3) }
  | bexpr  LEQ   bexpr      { Binop($1, Leq,    $3) }
  | bexpr  GT    bexpr      { Binop($1, Greater, $3)}
  | bexpr  GEQ   bexpr      { Binop($1, Geq,    $3) }

  /* equality */
  | bexpr  EQ    bexpr      { Binop($1, Equal, $3) }
  | bexpr  NEQ   bexpr      { Binop($1, Neq,   $3) }

  /* logical AND/OR */
  | bexpr  AND   bexpr      { Binop($1, And,   $3) }
  | bexpr  OR    bexpr      { Binop($1, Or,    $3) }



posexpr: 
  /* Literals */
    INTLIT                { IntLit($1) }
  | CHARLIT               { CharLit($1) }
  | FLOATLIT              { FloatLit($1) }
  | VECTORLIT             { VecLit($1) }
  | STRINGLIT             { StringLit($1)}

  /* primary expression */
  | ID                    { Id($1) }
  | LPAREN expr RPAREN    { $2 }

  /* postfix expression */
  | posexpr LBRACK expr RBRACK    %prec INDEX   { Index($1, $3) }
  | posexpr LPAREN arg_list RPAREN %prec CALL    { Call($1, List.rev $3) } 
  /* List.rev is used because in arg_list, expr_list is build from the back cause it is more efficient*/
  | posexpr DOT ID                %prec MEMB    { Member($1, $3) }
  | posexpr PLUSPLUS              %prec POST    { Posop(Postinc, $1) }
  | posexpr MINUSMINUS            %prec POST    { Posop(Postdec, $1) }

arg_list:
  /* nothing */       {[]}
 | expr_list          {$1}

/* Inverted List */
expr_list:
   expr               {[$1]}
 | expr_list COMMA expr    {$3::$1}
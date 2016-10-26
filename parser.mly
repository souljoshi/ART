/* Ocamlyacc parser for ART */

%{
  open Ast
%}

%token LPAREN RPAREN LBRACK RBRACK
%token DOT QMARK COLON COMMA SEMI
%token PLUS MINUS TIMES DIVIDE MOD
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN 
%token DIVASSIGN MODASSIGN PLUSPLUS MINUSMINUS
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token <int> INTLIT
%token <char> CHARLIT
%token <float> FLOATLIT
%token <float * float> VECTORLIT
%token <string> ID
%token EOF

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
%type <Ast.expr list> program

%%

program:
  expr_list EOF {$1}

expr_list:
  expr SEMI {[$1]}
  | expr_list expr SEMI {$2 :: $1}

expr:
  bexpr                     { $1 }

  /* Conditional */
  |bexpr QMARK expr COLON expr %prec CONDITIONAL { Trop(Cond, $1, $3, $5) }

  /* Assignment */
  |posexpr ASSIGN expr      { Binop($1, Asn, $3) } 
  |posexpr PLUSASSIGN expr  { Binop($1, AddAsn, $3) }
  |posexpr MINUSASSIGN expr { Binop($1, SubAsn, $3) }
  |posexpr TIMESASSIGN expr { Binop($1, MultAsn, $3) }
  |posexpr DIVASSIGN expr   { Binop($1, DivAsn, $3) }
  |posexpr MODASSIGN expr   { Binop($1, ModAsn, $3) }


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

  /* primary expression */
  | ID                    { Id($1) }
  | LPAREN expr RPAREN    { $2 }

  /* postfix expression */
  | posexpr LBRACK expr RBRACK    %prec INDEX   { Index($1, $3) }
  | posexpr LPAREN arglist RPAREN %prec CALL    { Call($1, $3) }
  | posexpr DOT ID                %prec MEMB    { Member($1, $3) }
  | posexpr PLUSPLUS              %prec POST    { Posop(Postinc, $1) }
  | posexpr MINUSMINUS            %prec POST    { Posop(Postdec, $1) }

arglist:
  /* nothing */       {[]}
 | expr               {[$1]}
 | arglist COMMA expr {$3::$1}

/* Ocamlyacc parser for ART */

%{
  open Ast
%}

%token LPAREN RPAREN LBRACK RBRACK
%token DOT QMARK COLON COMMA
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

%start expr
%type <Ast.expr> expr

%%

expr:
  bexpr                     {$1}

  /* Conditional */
  |bexpr QMARK expr COLON expr %prec CONDITIONAL {$1}

  /* Assignment */
  |posexpr ASSIGN expr      {$1} 
  |posexpr PLUSASSIGN expr  {$1}
  |posexpr MINUSASSIGN expr {$1}
  |posexpr TIMESASSIGN expr {$1}
  |posexpr DIVASSIGN expr   {$1}
  |posexpr MODASSIGN expr   {$1}


/* all expressions other than assignment/conditional */
bexpr:
  /* Postfix expressions */
  posexpr                  {$1}

  /* unary */
  | PLUS  bexpr %prec POS        {$2} /* { Unop(Neg, $2) } */
  | MINUS bexpr %prec NEG        {$2} /* { Unop(Neg, $2) } */
  | NOT bexpr                    {$2} /* { Unop(Not, $2) } */
  | PLUSPLUS   bexpr %prec PRE   {$2}
  | MINUSMINUS bexpr %prec PRE   {$2}

  /* multiplicative */
  | bexpr TIMES  bexpr      {$1} /* { Binop($1, Mult,  $3) } */
  | bexpr DIVIDE bexpr      {$1} /* { Binop($1, Div,   $3) } */
  | bexpr MOD    bexpr      {$1} /* { Binop($1, Div,   $3) } */

  /* additive */
  | bexpr PLUS   bexpr      {$1} /* { Binop($1, Add,   $3) } */
  | bexpr MINUS  bexpr      {$1} /* { Binop($1, Sub,   $3) } */

  /* relational */
  | bexpr  LT    bexpr      {$1} /* { Binop($1, Less,  $3) } */
  | bexpr  LEQ   bexpr      {$1} /* { Binop($1, Leq,   $3) } */
  | bexpr  GT    bexpr      {$1} /* { Binop($1, Greater, $3)} */
  | bexpr  GEQ   bexpr      {$1} /* { Binop($1, Geq,   $3) } *)/

  /* equality */
  | bexpr  EQ    bexpr      {$1} /* { Binop($1, Equal, $3) } */
  | bexpr  NEQ   bexpr      {$1} /* { Binop($1, Neq,   $3) } */

  /* logical AND/OR */
  | bexpr  AND   bexpr      {$1} /* { Binop($1, And,   $3) } */
  | bexpr  OR    bexpr      {$1} /* { Binop($1, Or,    $3) } */


posexpr: 
  /* Literals */
    INTLIT                {$1} /* { Literal($1) } */
  | CHARLIT               {$1}
  | FLOATLIT              {$1}
  | VECTORLIT             {$1}

  /* primary expression */
  | ID                    {$1} /* { Id($1) } */
  | LPAREN expr RPAREN    {$2} /* { $2 } */

  /* postfix expression */
  | posexpr LBRACK expr RBRACK    %prec INDEX   {$1}
  | posexpr LPAREN arglist RPAREN %prec CALL    {$1}
  | posexpr DOT ID                %prec MEMB    {$1}
  | posexpr PLUSPLUS              %prec POST    {$1}
  | posexpr MINUSMINUS            %prec POST    {$1}

arglist:
  /* nothing */       {[]}
 | expr               {[$1]}
 | arglist COMMA expr {$3::$1}

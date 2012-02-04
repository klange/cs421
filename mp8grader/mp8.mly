/* Use the expression datatype defined in expressions.ml: */
%{
    open Mp8common
%}

/* Define the tokens of the language: */
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> STRING IDENT
%token <(int*int)> OPCOM CLCOM SCLCOM
%token DBLSEMI PLUS MINUS TIMES DIV DPLUS DMINUS DTIMES DDIV CARAT EXP LT GEQ LEQ GT
       EQUALS AND OR PIPE ARROW DCOLON LET REC SEMI IN IF THEN ELSE FUN
       LBRAC RBRAC LPAREN RPAREN COMMA
       UNIT ERROR EOF
       TRY WITH UNDERSCORE RAISE 
       HEAD TAIL PRINT NEG FST SND

/* Define the "goal" nonterminal of the grammar: */
%start main
%type <Mp8common.toplvl> main

%left SEMI
%nonassoc LETREC
%nonassoc LET
%nonassoc FUN
%nonassoc IF
%left OR
%left AND
%left EQ LT GT LEQ GEQ
%right CONS
%left PLUS MINUS DPLUS DMINUS
%left DIV TIMES DDIV DTIMES
%right EXP
%nonassoc RAISE
%left APPLICATION

%%

pat:
  | UNDERSCORE	{ None }
  | INT		{ Some $1 }


main:
    expression DBLSEMI      			{ (Anon $1) }
  | LET IDENT EQUALS expression	DBLSEMI 	{ TopLet ($2,$4) }
  | LET REC IDENT IDENT EQUALS expression DBLSEMI  	{ (TopRec ($3, $4, $6)) }

expression: 
/* You will need to change stuff here, we just have a minimal version so that you can 
successfully make initially  */
  | atomic_expression { $1 }
  | LPAREN expression RPAREN { $2 }
  | LPAREN expression COMMA expression RPAREN { (BinExp (Comma, $2, $4)) }
  | LBRAC RBRAC { ConExp Nil }
  | LBRAC list_expression RBRAC { $2 }
  | list_mono_expression { $1 }
  | binop_expression { $1 }
  | LET IDENT EQUALS expression IN expression { LetExp($2, $4, $6) }
  | LET REC IDENT IDENT EQUALS expression IN expression %prec LETREC { RecExp($3, $4, $6, $8) }
  | expression expression %prec APPLICATION { AppExp($1, $2) }
  | FUN IDENT ARROW expression { FunExp($2, $4) }
  | IF expression THEN expression ELSE expression { IfExp($2, $4, $6) }
  | binary_expression { $1 }
  | RAISE expression { RaiseExp($2) }

list_expression:
  | expression { BinExp(Cons, $1, ConExp(Nil)) }
  | expression SEMI list_expression { BinExp(Cons, $1, $3) }

binary_expression:
  | expression AND expression { IfExp($1, $3, ConExp(Bool false)) }
  | expression OR  expression { IfExp($1, ConExp(Bool true), $3) }

list_mono_expression:
  | HEAD expression { MonExp (Head, $2) }
  | TAIL expression { MonExp (Tail, $2) }
  | FST  expression { MonExp (Fst,  $2) }
  | SND  expression { MonExp (Snd,  $2) }

binop_expression:
  | expression PLUS  expression { BinExp (Add, $1, $3) }
  | expression MINUS expression { BinExp (Sub, $1, $3) }
  | expression DIV   expression { BinExp (Div, $1, $3) }
  | expression TIMES expression { BinExp (Mul, $1, $3) }
  | expression DPLUS  expression { BinExp (FAdd, $1, $3) }
  | expression DMINUS expression { BinExp (FSub, $1, $3) }
  | expression DDIV   expression { BinExp (FDiv, $1, $3) }
  | expression DTIMES expression { BinExp (FMul, $1, $3) }
  | expression LT  expression { BinExp (Less, $1, $3) }
  | expression GT  expression { BinExp (Less, $3, $1) }
  | expression EQUALS  expression { BinExp (Eq, $3, $1) }
  | expression LEQ expression { IfExp (BinExp(Less, $1, $3), ConExp(Bool true), BinExp(Eq, $1, $3))  }
  | expression GEQ expression { IfExp (BinExp(Less, $3, $1), ConExp(Bool true), BinExp(Eq, $1, $3))  }
  | expression DCOLON expression { BinExp(Cons, $1, $3) }

atomic_expression: /* You may want to change this */
  | IDENT			{ VarExp $1 }
  | BOOL            { ConExp (Bool $1) }
  | INT { ConExp (Int $1) }
  | FLOAT { ConExp (Float $1) }
  | STRING { ConExp (String $1) }
  | UNIT { ConExp Unit }


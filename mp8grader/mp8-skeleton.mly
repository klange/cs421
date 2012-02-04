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
  atomic_expression { $1 }

atomic_expression: /* You may want to change this */
  | IDENT			{ VarExp $1 }


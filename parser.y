
%{
#include <stdlib.h>

#include "lib/types.h"

// TODO: Use a "push" parser instead of hardwiring this typedef.
//       https://stackoverflow.com/a/62823889/5712883
typedef void* yyscan_t;
%}

%code {
  void yyerror(YYLTYPE* yyllocp, yyscan_t unused, const char* msg);
}

%locations
%define api.value.type  { sch_object_t }
%define api.pure        full
%param                  { yyscan_t scanner }

%token TOKEN_LPAREN
%token TOKEN_RPAREN
%token TOKEN_LVECTOR
%token TOKEN_LBYTEVECTOR
%token TOKEN_QUOTE
%token TOKEN_QUASIQUOTE
%token TOKEN_UNQUOTE
%token TOKEN_UNQUOTE_SPLICING
%token TOKEN_DOT
%token TOKEN_DATUM_COMMENT
%token TOKEN_DIRECTIVE_FOLD_CASE
%token TOKEN_DIRECTIVE_NO_FOLD_CASE
%token TOKEN_LABEL_BINDING
%token TOKEN_LABEL_REFERENCE
/* `TOKEN_DATUM` does some heavy lifting and already represents
 * boolean, character, identifier (symbol), number, or string literals. */
%token TOKEN_DATUM

%start data

%%

data :                { $$ = NULL; }
     | data datum     { $$ = $1; }

datum : TOKEN_DATUM                                { $$ = $1; }
      | TOKEN_LPAREN           data  TOKEN_RPAREN  { $$ = SCH_NULL; $$ = make_list($2); }
      | TOKEN_LVECTOR          data  TOKEN_RPAREN  { $$ = make_vector($2); }
      | TOKEN_LBYTEVECTOR      bytes TOKEN_RPAREN  { $$ = make_bytevector($2); }
      | TOKEN_QUOTE            datum               { $$ = make_list("quote", $2); }
      | TOKEN_QUASIQUOTE       datum               { $$ = make_list("quasiquote", $2); }
      | TOKEN_UNQUOTE          datum               { $$ = make_list("unquote", $2); }
      | TOKEN_UNQUOTE_SPLICING datum               { $$ = make_list("unquote-splicing", $2); }
      | TOKEN_LABEL_BINDING    datum               { $$ = make_label_binding($1, $2); }
      | TOKEN_LABEL_REFERENCE                      { $$ = make_label_reference($1); }

bytes :                      { $$ = NULL; }
      | bytes TOKEN_DATUM    { /* TODO: Check that it's a small int. */ $$ = $1; }

%%

#include <stdlib.h>
#include <stdio.h>
void yyerror(YYLTYPE * yylloc, yyscan_t scanner, char const* msg) {
	fprintf(stderr, "yyerror: %s!\n", msg);
	exit(0);
}


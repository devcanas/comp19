%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "node.h"
extern int yylex();
int yyerror(char *s);
int yydebug = 1;
%}
%union {
	int i;			/* integer value */
	double r;		/* real value */
	char *s;		/* symbol name or string literal */
  Node *n;    /* AST node */
};
%token <i> tINT
%token <r> tREAL
%token <s> tID tSTR
%token tDO tWHILE tIF tTHEN tFOR tIN tUPTO tDOWNTO tSTEP tBREAK tCONTINUE
%token tVOID tINTEGER tSTRING tNUMBER tCONST tPUBLIC tINCR tDECR
%token tATR tNE tGE tLE tELSE

%nonassoc       IF_EXPRESSION
%nonassoc       tELSE
%right          tATR
%left           '|'
%left           '&'
%nonassoc       '~'
%left           '=' tNE
%left           '<' '>' tLE tGE
%left           '+' '-'
%left           '*' '/' '%'
%nonassoc       tUASTERISC tUAMPERSAND '!' tUMINUS '++' '--'
%nonassoc       '(' ')' '[' ']'

%type <n> declarations declaration expression left_value empty arguments parameters parameter type
%type <n> pointer opt_public opt_const opt_init opt_body
%type <n> init body opt_parameter opt_instruction instruction opt_step_expression opt_integer opt_semi_colon

%token DECLS DECL PARAM ARG INSTR TYPE INDEX CALL ALLOC RANGE BODY FUNC VAR

%%
file : declarations     { printNode($1, 0, 0); freeNode($1); }
     ;

declarations : empty                        { $$ = $1; }       
             | declarations declaration     { $$ = binNode(DECLS, $1, $2); }
             ;

declaration : opt_public opt_const type tID opt_init ';'              
                              { $$ = binNode(tPUBLIC, $1, binNode(tCONST, $2, binNode(TYPE, $3, binNode(DECL, strNode(tID, $4), $5)))); }
            | opt_public opt_const type pointer tID opt_init ';'
                              { $$ = binNode(DECL, strNode(tID, $5), $6); }
            ;

init : tATR tINT                      { $$ = binNode(VAR, nilNode(tATR), intNode(tINT, $2)); }
     | tATR opt_const tSTR            { $$ = binNode(VAR, nilNode(tATR), binNode(tCONST, $2, strNode(tSTR, $3))); }
     | tATR tREAL                     { $$ = binNode(VAR, nilNode(tATR), realNode(tREAL, $2)); }
     | tATR tID                       { $$ = binNode(VAR, nilNode(tATR), strNode(tID, $2)); }
     | '(' parameters ')' opt_body    { $$ = binNode(FUNC, $2, $4); }
     ;

body : '{' { IDPush(); } opt_parameter opt_instruction '}'  { $$ = binNode(BODY, $2, $3); IDPop(); }
     ;

instruction : tIF expression tTHEN instruction %prec IF_EXPRESSION
                          { $$ = binNode(tIF, $2, $4); }
            | tIF expression tTHEN instruction tELSE instruction
                          { $$ = binNode(tELSE, binNode(tIF, $2, $4), $6); }
            | tDO instruction tWHILE expression ';'
                          { $$ = binNode(tWHILE, uniNode(tDO, $2), $4); }
            | tFOR left_value tIN expression tUPTO expression opt_step_expression tDO instruction
                          { $$ = binNode(tFOR, $2, binNode(tDO, binNode(tSTEP, binNode(RANGE, uniNode(tIN, $4), uniNode(tUPTO, $6)), $7), $9)); }
            | tFOR left_value tIN expression tDOWNTO expression opt_step_expression tDO instruction
                          { $$ = binNode(tFOR, $2, binNode(tDO, binNode(tSTEP, binNode(RANGE, uniNode(tIN, $4), uniNode(tDOWNTO, $6)), $7), $9)); }
            | expression ';'                  { $$ = $1; }
            | body                            { $$ = $1; }
            | tBREAK opt_integer ';'          { $$ = binNode(tBREAK, nilNode(tBREAK), $2); }
            | tCONTINUE opt_integer ';'       { $$ = binNode(tCONTINUE, nilNode(tCONTINUE), $2); }
            | left_value '#' expression ';'   { $$ = binNode(ALLOC, $3, $1); }
            ;

left_value : tID                        { $$ = strNode(tID, $1); }
           | tID '[' expression ']'     { $$ = binNode(INDEX, strNode(tID, $1), $3); }
           | pointer left_value         { $$ = uniNode('*', $2); }
           ;

expression : left_value                         { $$ = $1; }
           | tREAL                              { $$ = realNode(tREAL, $1); }
           | tINT                               { $$ = intNode(tINT, $1); }
           | tSTR                               { $$ = strNode(tSTR, $1); }
           | '(' expression ')'                 { $$ = $2; }
           | expression '(' arguments ')'       { $$ = binNode(CALL, $3, $1); }
           | '-' expression %prec tUMINUS       { $$ = uniNode(tUMINUS, $2); }
           | expression '!'                     { $$ = uniNode('!', $1); }
           | '&' left_value %prec tUAMPERSAND   { $$ = uniNode(tUAMPERSAND, $2); }
           | left_value tDECR                   { $$ = binNode(tDECR, $1, intNode(tINT, 1)); }
           | left_value tINCR                   { $$ = binNode(tINCR, $1, intNode(tINT, 1)); }
           | tINCR left_value                   { $$ = binNode(tDECR, intNode(tINT, 1), $2); }
           | tDECR left_value                   { $$ = binNode(tINCR, intNode(tINT, 1), $2); }
           | expression '*' expression          { $$ = binNode('*', $1, $3); }
           | expression '%' expression          { $$ = binNode('%', $1, $3); }
           | expression '/' expression          { $$ = binNode('/', $1, $3); }
           | expression '+' expression          { $$ = binNode(12, $1, $3); }
           | expression '-' expression          { $$ = binNode('-', $1, $3); }
           | expression '>' expression          { $$ = binNode('>', $1, $3); }
           | expression '<' expression          { $$ = binNode('<', $1, $3); }
           | expression tGE expression          { $$ = binNode(tGE, $1, $3); }
           | expression tLE expression          { $$ = binNode(tLE, $1, $3); }
           | expression '=' expression          { $$ = binNode('=', $1, $3); }
           | expression tNE expression          { $$ = binNode(tNE, $1, $3); }
           | '~' expression                     { $$ = uniNode('~', $2); }
           | expression '&' expression          { $$ = binNode('&', $1, $3); }
           | expression '|' expression          { $$ = binNode('|', $1, $3); }
           | left_value tATR expression         { $$ = binNode(tATR, $3, $1); }
           ;

arguments : empty                               { $$ = $1; }
          | expression                          { $$ = binNode(ARG, $1, nilNode(0)); }
          | arguments ',' expression            { $$ = binNode(ARG, $3, $1); } 
          ;

parameters : empty                          { $$ = $1; }
           | parameter                      { $$ = binNode(PARAM, $1, nilNode(0)); }
           | parameters ',' parameter       { $$ = binNode(PARAM, $3, $1); }
           ;

parameter : type tID opt_semi_colon         { $$ = binNode(PARAM, $1, strNode(tID, $2)); }
          | type '*' tID opt_semi_colon     { $$ = binNode(PARAM, $1, uniNode('*', strNode(tID, $3))); }
          ;

pointer : '*' %prec tUASTERISC;    { $$ = nilNode(0); }

type : tINTEGER     { $$ = nilNode(tINTEGER); }
     | tSTRING      { $$ = nilNode(tSTRING); }
     | tNUMBER      { $$ = nilNode(tNUMBER); }
     | tVOID        { $$ = nilNode(tVOID); }
     ;

/* Optional (and recursive) Productions */

opt_public : empty      { $$ = $1; }
           | tPUBLIC    { $$ = strNode(tPUBLIC, "public"); }
           ;

opt_const : empty       { $$ = $1; }
          | tCONST      { $$ = nilNode(tCONST); }
          ;

opt_init : empty        { $$ = $1; }
         | init         { $$ = $1; }
         ;

opt_body : empty        { $$ = $1; } 
         | body         { $$ = $1; } 
         ;

opt_parameter : empty                       { $$ = $1; }
              | opt_parameter parameter     { $$ = binNode(PARAM, $1, $2); }
              ;

opt_instruction : empty                         { $$ = $1; }
                | opt_instruction instruction   { $$ = binNode(INSTR, $1, $2); }
                ;

opt_step_expression : empty                     { $$ = $1; }
                    | tSTEP expression          { $$ = uniNode(tSTEP, $2); }
                    ;

opt_integer : empty            { $$ = $1; }
            | tINTEGER         { $$ = nilNode(tINTEGER); }
            ;

opt_semi_colon : empty         { $$ = $1; }
               | ';'           { $$ = nilNode(0); }
               ;

empty :      { $$ = nilNode(0); }
      ;

%%
int yyerror(char *s) { printf("%s\n", s); return 1; }
char *dupstr(const char*s) { return strdup(s); }
int main(int argc, char *argv[]) { return yyparse(); }
%{
#include <stdio.h>
#include <stdlib.h>
#include "y.tab.h"
#ifndef YYERRCODE
#define YYERRCODE 256
#endif
#define YYDEBUG 1
extern int yylineno;
int yylex(); 
%}

%union {
    int i; 
	double d;
    char *s; 
}

%token<i> tINTEGER tOCTAL
%token<d> tNUMBER
%token<s> tSTRING tIDENTIFIER
%token tPUBLIC tCONST tVOID tENTRY 
%token tINTEGER_TYPE tSTRING_TYPE tNUMBER_TYPE tOCTAL_TYPE tBINARY_TYPE
%token tIF tTHEN tELSE tDO tWHILE tFOR tIN tUPTO tDOWNTO tSTEP tBREAK tCONTINUE
%token tEQ tNE tLE tGE tINCR tDECR tASSIGN

%%
file: ;
%%
int yyerror(char *s) { fprintf(stderr, "%d: %s\n", yylineno, s); return 0; }

int main(int argc, char *argv[]) {
	extern YYSTYPE yylval;
	int tk;
	while ((tk = yylex())) 
	    if (tk > YYERRCODE)
		    printf("%d:\t%s\n", tk, yyname[tk]);
		else
			printf("%d:\t%c\n", tk, tk);
	return 0;
}

%option stack
%{
#include "y.tab.h"
#include <string.h>
int decimal(char *s);
int octal(char *s);
int yyerror(char *s);
%}
%x X_COMMENT X_STRING
%%

==.*                        ;                   /* Comments handling */
\/\*					    yy_push_state(X_COMMENT);
<X_COMMENT>.|\n		        ;
<X_COMMENT>"*/"             yy_pop_state();

:=              return tASSIGN;     /* Assignement operator */

\=              return tEQ;         /* Comparison operators */
\<\>            return tNE;
\<\=            return tLE;
\>\=            return tGE;

public          return tPUBLIC;     /* DIY Reserved words */
const           return tCONST;
void            return tVOID;
entry           return tENTRY;

if              return tIF;         /* Control flow */
then            return tTHEN;
else            return tELSE;
do              return tDO;
while           return tWHILE;
for             return tFOR;
in              return tIN;
upto            return tUPTO;
downto          return tDOWNTO;
step            return tSTEP;
break           return tBREAK;
continue        return tCONTINUE;

[A-Za-z_][A-Za-z0-9_]*	    yylval.s = strdup(yytext); return tIDENTIFIER;
[1-9][0-9]+			        yylval.i = decimal(yytext); return tINTEGER;
0[0-9]*                     yylval.i = octal(yytext); return tOCTAL;

\"			                yy_push_state(X_STRING);
<X_STRING>\"			    yy_pop_state(); return tSTRING;
<X_STRING>"*n"		        strcat(yylval.s, "\n");
<X_STRING>"*t"		        strcat(yylval.s, "\t");
<X_STRING>"*0"		        strcat(yylval.s, "\0");
<X_STRING>"*("		        strcat(yylval.s, "{");
<X_STRING>"*)"		        strcat(yylval.s, "}");
<X_STRING>\*[*'"]           strcat(yylval.s, yytext+1);
<X_STRING>\*.		        yyerror("invalid escape sequence in string");
<X_STRING>.|\n              strcat(yylval.s, yytext);

[-()<>=+*/%;{},.~&|?\[\]\'\"\\!]    return *yytext;

[ \t\n\r]+		            ; /* ignore whitespace */
 
.  yyerror("Unknown character");

%%
int yywrap() { return 1; }

int decimal(char *text) {
    errno = 0;
    int a = strtol(text, 0, 10);
	if (errno == ERANGE) yyerror("decimal overflow");
    return a;
}

int octal(char *s)
{
  int i, a = 0, b = 0;

  for (i = 0; i < strlen(s); i++) {
    if (s[i] < '0' || s[i] > '9') break;
    b = b * 8 + s[i] - '0';
    if (b < a) {
      yyerror("octal overflow");
      break;
    }
    a = b;
  }
  return a;
}